#!/bin/bash

usage() {
    cat <<EOF
Usage: $0 [-h]

This is the release script for Membrane RTC Engine.
Run \`$0\`, then follow the instructions.

Available options:
    -h    Print this help and exit
EOF
}

## Global variables
# The order of these repos is important: Engine must be released first, then WebRTC, then the rest
REPOS="engine webrtc"
for elixir_repo in */mix.exs; do
    repo="${elixir_repo%/mix.exs}"
    [[ "$repo" =~ engine|webrtc|integration_test ]] || REPOS+=" $repo"
done

# How many stages the release consists of (how many times does this script have to be run)
MAX_STAGE="3"

## Config
set -eo pipefail
trap handle_interrupt SIGINT

## Handlers
handle_interrupt() {
    echo "[RELEASE] Interrupt. Exiting"
    exit 2
}

## Helpers
# Read current version of repo
read_version() {
    REPO="$1"
    sed -E -n 's/^.*@version "(.*)".*$/\1/p' "$REPO"/mix.exs
}

# Get the next version (strip "-dev" if present, otherwise bump minor and append "-dev")
next_version() {
    OLD_VERSION="$1"

    if [[ "$OLD_VERSION" =~ -dev$ ]]; then
        echo "${OLD_VERSION%-dev}"
    else
        MINOR=$(echo "$OLD_VERSION" | sed -n -E 's/^[0-9]+\.([0-9]+)\.[0-9]+$/\1/p')
        echo "0.$(( $MINOR + 1 )).0-dev"
    fi
}

# Get the hex package name of repo
package() {
    REPO="$1"
    [[ "$REPO" == "engine" ]] && echo "membrane_rtc_engine" || echo "membrane_rtc_engine_$REPO"
}

# Create a mix deps list entry from a given repo and version (path entry if version=="path")
dep() {
    REPO="$1"
    VERSION="$2"

    PACKAGE=$(package $REPO)
    [[ "$VERSION" == "path" ]] && echo "{:$PACKAGE, path: \"..\/$REPO\"}" || echo "{:$PACKAGE, \"~> $VERSION\"}"
}

# Persistence
get_stage() {
    [[ -f ./RELEASE_STAGE ]] && cat ./RELEASE_STAGE || echo "0"
}

set_stage() {
    STAGE="$1"
    [[ "$STAGE" == "$MAX_STAGE" ]] && rm -f ./RELEASE_STAGE || echo "$STAGE" > ./RELEASE_STAGE
}

## Other utils
confirm_execute() {
    echo "[RELEASE CONFIRM-EXECUTE] I'm about to execute the following command:" "$@"
    read -p "Proceed? [y/n] " CONFIRM

    if [[ "$CONFIRM" =~ (y|Y)(es|ES)? ]]; then
        "$@"
    elif [[ "$CONFIRM" =~ (n|N)(o|O)? ]]; then
        read -p "Terminate the script? [y/n] " TERMINATE
        if [[ "$TERMINATE" =~ (y|Y)(es|ES)? ]]; then
            echo "[RELEASE CONFIRM-EXECUTE] Abort."
            exit 2
        fi
    else
        echo "Please type 'y' or 'n'"
        confirm_execute $@
    fi
}

create_branch() {
    BRANCH="$1"

    # Delete the branch if it exists, but don't exit the script if it doesn't exist
    git branch -D "$BRANCH" || true
    git checkout -b "$BRANCH"
}

commit_and_push() {
    MESSAGE="$1"
    BRANCH="${2-}"

    git add .
    git status
    git commit -m "$MESSAGE"
    [[ "$BRANCH" == "" ]] && ARGS="" || ARGS="-u $(git remote) $BRANCH"
    confirm_execute git push $ARGS
}


## Release stages
stage1() {
    echo "[RELEASE STAGE 1] 1. Pulling latest changes from 'master'"
    git checkout master
    git pull

    NEW_ENGINE_VERSION=$(next_version $(read_version engine))
    NEW_WEBRTC_VERSION=$(next_version $(read_version webrtc))
    NEW_ENGINE_DEP=$(dep engine $NEW_ENGINE_VERSION)
    NEW_WEBRTC_DEP=$(dep webrtc $NEW_WEBRTC_VERSION)

    BRANCH="release-v$NEW_ENGINE_VERSION"
    echo "[RELEASE STAGE 1] 2. Checking out a new branch '$BRANCH'"
    create_branch "$BRANCH"


    # special handling of two READMEs which include both libs as an example
    echo "[RELEASE STAGE 1] 3. Updating versions in main READMEs "
    sed -i '' -E "s/$(dep engine ".*")/$NEW_ENGINE_DEP/" README.md engine/README.md
    sed -i '' -E "s/$(dep webrtc ".*")/$NEW_WEBRTC_DEP/" README.md engine/README.md


    echo "[RELEASE STAGE 1] 4. Updating repos"
    for repo in $REPOS; do
        echo "[RELEASE STAGE 1 $repo] 1. Updating deps"
        cd $repo && mix deps.update --all && cd ..
    done

    for repo in $REPOS; do
        old_version=$(read_version $repo)
        new_version=$(next_version $old_version)
        echo "[RELEASE STAGE 1 $repo] 2. Changing version from $old_version to $new_version"
        sed -i '' -E "s/@version \"$old_version\"/@version \"$new_version\"/" $repo/mix.exs
        sed -i '' -E "s/^## $old_version/## $new_version/" $repo/CHANGELOG.md
        sed -i '' -E "s/$(dep $repo ".*")/$(dep $repo $new_version)/" $repo/README.md

        echo "[RELEASE STAGE 1 $repo] 3. Changing Engine deps from paths to hex packages"
        if [[ "$repo" != "engine" ]]; then
            sed -i '' -E "s/$(dep engine path)/$NEW_ENGINE_DEP/" $repo/mix.exs

            if [[ "$repo" != "webrtc" ]]; then
                sed -i '' -E "s/$(dep webrtc path)/$NEW_WEBRTC_DEP/" $repo/mix.exs
            fi
        fi
    done


    echo "[RELEASE STAGE 1] 5. Committing and pushing changes"
    commit_and_push "Release engine $NEW_ENGINE_VERSION" "$BRANCH"


    cat <<EOF

[RELEASE] YOUR TURN:
    0. Double-check my work!
    1. Make a PR with these changes (CI won't pass)
    2. Get it approved, but don't merge
    3. Run \`$0\` again
EOF
}


stage2() {
    echo "[RELEASE STAGE 2] 1. Releasing repos"
    for repo in $REPOS; do
        cd $repo

        echo "[RELEASE STAGE 2 $repo] 1. Updating deps to fetch freshly released Engine packages"
        mix deps.update --all

        echo "[RELEASE STAGE 2 $repo] 2. RELEASING"
        mix hex.publish

        cd ..
    done


    echo "[RELEASE STAGE 2] 2. Committing and pushing changes to the lockfiles"
    commit_and_push "Update lockfiles"


    cat <<EOF

[RELEASE] YOUR TURN:
    0. Double-check my work!
    1. Wait for CI to pass (hopefully)
    2. Merge the PR
    3. Run \`$0\` again
EOF
}


stage3() {
    echo "[RELEASE STAGE 3] 1. Pulling latest changes from 'master'"
    git checkout master
    git pull


    echo "[RELEASE STAGE 3] 2. Tagging repos"
    tags=""
    for repo in $REPOS; do
        tag="$repo-v$(read_version $repo)"
        echo "[RELEASE STAGE 3 $repo] Adding tag $tag"
        git tag $tag
        tags+=" $tag"
    done


    echo "[RELEASE STAGE 3] 3. Pushing the tags"
    confirm_execute git push $(git remote) $tags


    BRANCH="post-release"
    echo "[RELEASE STAGE 3] 4. Checking out a new branch '$BRANCH'"
    create_branch "$BRANCH"


    echo "[RELEASE STAGE 3] 5. Updating repos"
    for repo in $REPOS; do
        old_version=$(read_version $repo)
        new_version=$(next_version $old_version)
        echo "[RELEASE STAGE 3 $repo] 1. Changing version from $old_version to $new_version"
        sed -i '' -E "s/@version \"$old_version\"/@version \"$new_version\"/" $repo/mix.exs

        echo "[RELEASE STAGE 3 $repo] 2. Adding entry to changelog"
        printf "# Changelog\n\n## $new_version\n" > $repo/CHANGELOG-GEN.md
        tail -n+2 $repo/CHANGELOG.md >> $repo/CHANGELOG-GEN.md
        mv $repo/CHANGELOG-GEN.md $repo/CHANGELOG.md

        echo "[RELEASE STAGE 3 $repo] 3. Changing Engine deps from hex packages to paths"
        if [[ "$repo" != "engine" ]]; then
            sed -i '' -E "s/$(dep engine ".*")/$(dep engine path)/" $repo/mix.exs

            if [[ "$repo" != "webrtc" ]]; then
                sed -i '' -E "s/$(dep webrtc ".*")/$(dep webrtc path)/" $repo/mix.exs
            fi
        fi
    done


    echo "[RELEASE STAGE 3] 6. Committing and pushing changes"
    commit_and_push "Post-release changes" "$BRANCH"


    cat <<EOF

[RELEASE] YOUR TURN:
    0. Double-check my work!
    1. Make a PR with these changes
    2. Merge it after getting approvals (no rush)
EOF
}


case "$1" in
    -h|--help)
        usage && exit 0 ;;
esac

if [[ -n "$(git status --porcelain)" ]]; then
    echo "[RELEASE] Uncommitted changes detected! Aborting"
    exit 1
fi

STAGE=$(( $(get_stage) + 1 ))
echo "[RELEASE] Executing stage $STAGE"

case $STAGE in
    1)
        stage1 ;;
    2)
        stage2 ;;
    3)
        stage3 ;;
esac

set_stage $STAGE
echo "[RELEASE] Stage $STAGE complete!"

[[ "$STAGE" == "$MAX_STAGE"  ]] && echo "[RELEASE] Finished!"

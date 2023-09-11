#include "parser.h"

void handle_destroy_state(UnifexEnv *env, State *state) {
  UNIFEX_UNUSED(env);

  if (state->parser_ctx != NULL) {
    av_parser_close(state->parser_ctx);
  }

  if (state->codec_ctx != NULL) {
    avcodec_free_context(&state->codec_ctx);
  }
}

UNIFEX_TERM create(UnifexEnv *env) {
  UNIFEX_TERM res;
  State *state = unifex_alloc_state(env);
  state->codec_ctx = NULL;
  state->parser_ctx = NULL;
  state->last_frame_number = -1;
  state->poc_offset = 0;

  const AVCodec *codec = avcodec_find_decoder(AV_CODEC_ID_H264);
  if (!codec) {
    res = create_result_error(env, "nocodec");
    goto exit_create;
  }

  state->parser_ctx = av_parser_init(codec->id);
  if (!state->parser_ctx) {
    res = create_result_error(env, "noparser");
    goto exit_create;
  }

  state->codec_ctx = avcodec_alloc_context3(codec);
  if (!state->codec_ctx) {
    res = create_result_error(env, "codec_alloc");
    goto exit_create;
  }

  if (avcodec_open2(state->codec_ctx, codec, NULL) < 0) {
    res = create_result_error(env, "codec_open");
    goto exit_create;
  }

  res = create_result_ok(env, state);
exit_create:
  unifex_release_state(env, state);
  return res;
}

static int resolution_changed(resolution last_res, State *state) {
  // consider only width and height in metadata comparison
  return (state->parser_ctx->width != last_res.width) ||
         (state->parser_ctx->height != last_res.height);
}

void update_last_frame_number(int poc, State *state) {
  state->last_frame_number += 1;
  if (poc == 0) {
    state->poc_offset = state->last_frame_number;
  }
}

UNIFEX_TERM parse(UnifexEnv *env, UnifexPayload *payload, State *state) {
  UNIFEX_TERM res_term;
  int ret;
  size_t max_frames = 32, frames_cnt = 0, max_changes = 5, changes_cnt = 0;
  AVPacket *pkt = NULL;

  unsigned *out_frame_sizes = unifex_alloc(max_frames * sizeof(unsigned));
  int *presentation_order_numbers = unifex_alloc(max_frames * sizeof(int));
  int *decoding_order_numbers = unifex_alloc(max_frames * sizeof(int));
  resolution *changes = unifex_alloc(max_changes * sizeof(resolution));
  uint8_t *parser_input_buffer =
      unifex_alloc(payload->size + AV_INPUT_BUFFER_PADDING_SIZE);
  pkt = av_packet_alloc();

  if (!out_frame_sizes || !changes || !parser_input_buffer || !pkt) {
    res_term = parse_result_error(env, "mem_alloc");
    goto exit_parse_frames;
  }

  memcpy(parser_input_buffer, payload->data, payload->size);
  memset(parser_input_buffer + payload->size, 0, AV_INPUT_BUFFER_PADDING_SIZE);

  uint8_t *data_ptr = parser_input_buffer;
  size_t data_left = payload->size;
  resolution *last_res =
      &(resolution){state->parser_ctx->width, state->parser_ctx->height, 0};
  while (data_left > 0) {
    ret = av_parser_parse2(state->parser_ctx, state->codec_ctx, &pkt->data,
                           &pkt->size, data_ptr, data_left, AV_NOPTS_VALUE,
                           AV_NOPTS_VALUE, 0);

    if (ret < 0) {
      res_term = parse_result_error(env, "parsing");
      goto exit_parse_frames;
    }

    data_ptr += ret;
    data_left -= ret;

    if (pkt->size > 0) {
      if (resolution_changed(*last_res, state)) {
        if (changes_cnt >= max_changes) {
          max_changes *= 2;
          changes = unifex_realloc(changes, max_changes * sizeof(resolution));
        }

        resolution new_res = {state->parser_ctx->width,
                              state->parser_ctx->height, frames_cnt};
        changes[changes_cnt++] = new_res;

        last_res = &new_res;
      }

      if (frames_cnt >= max_frames) {
        max_frames *= 2;
        out_frame_sizes =
            unifex_realloc(out_frame_sizes, max_frames * sizeof(unsigned));
        decoding_order_numbers =
            unifex_realloc(decoding_order_numbers, max_frames * sizeof(int));
        presentation_order_numbers = unifex_realloc(presentation_order_numbers,
                                                    max_frames * sizeof(int));
      }
      out_frame_sizes[frames_cnt] = pkt->size;
      // "Note 2: the JM reference encoder increments POC by 2 for every
      // complete frame." from
      // https://www.vcodex.com/h264avc-picture-management/
      int picture_order_number = state->parser_ctx->output_picture_number / 2;
      // If frame with POC=0 was not encounter yet presentation order number
      // cannot be calculated reliably.
      if (state->last_frame_number != -1 || picture_order_number == 0) {
        update_last_frame_number(picture_order_number, state);
        presentation_order_numbers[frames_cnt] =
            picture_order_number + state->poc_offset;
        decoding_order_numbers[frames_cnt] = state->last_frame_number;
      } else {
        presentation_order_numbers[frames_cnt] = -1;
        decoding_order_numbers[frames_cnt] = -1;
      }

      frames_cnt++;
    }
  }

  res_term = parse_result_ok(
      env, out_frame_sizes, frames_cnt, decoding_order_numbers, frames_cnt,
      presentation_order_numbers, frames_cnt, changes, changes_cnt);
exit_parse_frames:
  unifex_free(out_frame_sizes);
  unifex_free(decoding_order_numbers);
  unifex_free(presentation_order_numbers);
  unifex_free(parser_input_buffer);
  unifex_free(changes);
  av_packet_free(&pkt);
  return res_term;
}

UNIFEX_TERM get_profile(UnifexEnv *env, State *state) {
  char *profile_atom;

  switch (state->codec_ctx->profile) {
  case FF_PROFILE_H264_CONSTRAINED_BASELINE:
    profile_atom = "constrained_baseline";
    break;
  case FF_PROFILE_H264_BASELINE:
    profile_atom = "baseline";
    break;
  case FF_PROFILE_H264_MAIN:
    profile_atom = "main";
    break;
  case FF_PROFILE_H264_HIGH:
    profile_atom = "high";
    break;
  case FF_PROFILE_H264_HIGH_10:
    profile_atom = "high_10";
    break;
  case FF_PROFILE_H264_HIGH_422:
    profile_atom = "high_422";
    break;
  case FF_PROFILE_H264_HIGH_444:
    profile_atom = "high_444";
    break;
  case FF_PROFILE_H264_HIGH_10_INTRA:
    profile_atom = "high_10_intra";
    break;
  case FF_PROFILE_H264_HIGH_422_INTRA:
    profile_atom = "high_422_intra";
    break;
  case FF_PROFILE_H264_HIGH_444_INTRA:
    profile_atom = "high_444_intra";
    break;
  default:
    profile_atom = "unknown";
  }

  return get_profile_result_ok(env, profile_atom);
}

UNIFEX_TERM flush(UnifexEnv *env, State *state) {
  int ret;
  uint8_t *data_ptr;
  unsigned out_frame_size;
  resolution res = {state->parser_ctx->width, state->parser_ctx->height, 0};

  ret = av_parser_parse2(state->parser_ctx, state->codec_ctx, &data_ptr,
                         (int *)&out_frame_size, NULL, 0, AV_NOPTS_VALUE,
                         AV_NOPTS_VALUE, 0);
  if (ret < 0) {
    return parse_result_error(env, "parsing");
  }

  if (out_frame_size == 0) {
    return flush_result_ok(env, NULL, 0, NULL, 0, NULL, 0, res);
  }

  // "Note 2: the JM reference encoder increments POC by 2 for every complete
  // frame." from https://www.vcodex.com/h264avc-picture-management/
  int picture_order_number = state->parser_ctx->output_picture_number / 2;
  update_last_frame_number(picture_order_number, state);

  int presentation_order_number = picture_order_number + state->poc_offset;
  int decoding_order_number = state->last_frame_number;

  if (resolution_changed(res, state)) {
    res.width = state->parser_ctx->width;
    res.height = state->parser_ctx->height;
  }

  return flush_result_ok(env, &out_frame_size, 1, &decoding_order_number, 1,
                         &presentation_order_number, 1, res);
}

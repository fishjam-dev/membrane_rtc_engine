#include "decoder.h"

void handle_destroy_state(UnifexEnv *env, State *state) {
  UNIFEX_UNUSED(env);

  if (state->codec_ctx != NULL) {
    avcodec_free_context(&state->codec_ctx);
  }
}

UNIFEX_TERM create(UnifexEnv *env) {
  UNIFEX_TERM res;
  State *state = unifex_alloc_state(env);
  state->codec_ctx = NULL;

#if (LIBAVCODEC_VERSION_MAJOR < 58)
  avcodec_register_all();
#endif
  const AVCodec *codec = avcodec_find_decoder(AV_CODEC_ID_H264);
  if (!codec) {
    res = create_result_error(env, "nocodec");
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

static int get_frames(UnifexEnv *env, AVPacket *pkt,
                      UnifexPayload ***ret_frames,
                      int64_t **best_effort_timestamps, int *max_frames,
                      int *frame_cnt, int use_shm, State *state) {
  AVFrame *frame = av_frame_alloc();
  UnifexPayload **frames = unifex_alloc((*max_frames) * sizeof(*frames));
  int64_t *timestamps = unifex_alloc((*max_frames) * sizeof(*timestamps));

  int ret = avcodec_send_packet(state->codec_ctx, pkt);
  if (ret < 0) {
    ret = DECODER_SEND_PKT_ERROR;
    goto exit_get_frames;
  }

  ret = avcodec_receive_frame(state->codec_ctx, frame);
  while (ret != AVERROR(EAGAIN) && ret != AVERROR_EOF) {
    if (ret < 0) {
      ret = DECODER_DECODE_ERROR;
      goto exit_get_frames;
    }

    if (*frame_cnt >= (*max_frames)) {
      *max_frames *= 2;
      frames = unifex_realloc(frames, (*max_frames) * sizeof(*frames));
      timestamps =
          unifex_realloc(timestamps, (*max_frames) * sizeof(*timestamps));
    }

    size_t payload_size = av_image_get_buffer_size(
        state->codec_ctx->pix_fmt, frame->width, frame->height, 1);

    frames[*frame_cnt] = unifex_alloc(sizeof(UnifexPayload));

    UnifexPayloadType payload_type;
    if (use_shm) {
      payload_type = UNIFEX_PAYLOAD_SHM;
    } else {
      payload_type = UNIFEX_PAYLOAD_BINARY;
    }
    unifex_payload_alloc(env, payload_type, payload_size, frames[*frame_cnt]);
    timestamps[*frame_cnt] = frame->best_effort_timestamp;

    av_image_copy_to_buffer(
        frames[*frame_cnt]->data, payload_size,
        (const uint8_t *const *)frame->data, (const int *)frame->linesize,
        state->codec_ctx->pix_fmt, frame->width, frame->height, 1);

    (*frame_cnt)++;

    ret = avcodec_receive_frame(state->codec_ctx, frame);
  }
  ret = 0;
exit_get_frames:
  *ret_frames = frames;
  *best_effort_timestamps = timestamps;
  av_frame_free(&frame);
  return ret;
}

UNIFEX_TERM decode(UnifexEnv *env, UnifexPayload *payload, int64_t pts,
                   int64_t dts, int use_shm, State *state) {
  UNIFEX_TERM res_term;
  AVPacket *pkt = NULL;
  int max_frames = 16, frame_cnt = 0;
  UnifexPayload **out_frames = NULL;
  int64_t *pts_list = NULL;
  pkt = av_packet_alloc();
  pkt->data = payload->data;
  pkt->size = payload->size;
  pkt->dts = dts;
  pkt->pts = pts;

  int ret = 0;
  if (pkt->size > 0) {
    ret = get_frames(env, pkt, &out_frames, &pts_list, &max_frames, &frame_cnt,
                     use_shm, state);
  }

  switch (ret) {
  case DECODER_SEND_PKT_ERROR:
    res_term = decode_result_error(env, "send_pkt");
    break;
  case DECODER_DECODE_ERROR:
    res_term = decode_result_error(env, "decode");
    break;
  default:
    res_term =
        decode_result_ok(env, pts_list, frame_cnt, out_frames, frame_cnt);
  }

  if (out_frames != NULL) {
    for (int i = 0; i < frame_cnt; i++) {
      if (out_frames[i] != NULL) {
        unifex_payload_release(out_frames[i]);
        unifex_free(out_frames[i]);
      }
    }
    unifex_free(out_frames);
  }
  if (pts_list != NULL) {
    unifex_free(pts_list);
  }

  av_packet_free(&pkt);
  return res_term;
}

UNIFEX_TERM flush(UnifexEnv *env, int use_shm, State *state) {
  UNIFEX_TERM res_term;
  int max_frames = 8, frame_cnt = 0;
  UnifexPayload **out_frames = NULL;
  int64_t *pts_list = NULL;

  int ret = get_frames(env, NULL, &out_frames, &pts_list, &max_frames,
                       &frame_cnt, use_shm, state);

  switch (ret) {
  case DECODER_SEND_PKT_ERROR:
    res_term = flush_result_error(env, "send_pkt");
    break;
  case DECODER_DECODE_ERROR:
    res_term = flush_result_error(env, "decode");
    break;
  default:
    res_term = flush_result_ok(env, pts_list, frame_cnt, out_frames, frame_cnt);
  }

  if (out_frames != NULL) {
    for (int i = 0; i < frame_cnt; i++) {
      if (out_frames[i] != NULL) {
        unifex_payload_release(out_frames[i]);
        unifex_free(out_frames[i]);
      }
    }
    unifex_free(out_frames);
  }
  if (pts_list != NULL) {
    unifex_free(pts_list);
  }

  return res_term;
}

UNIFEX_TERM get_metadata(UnifexEnv *env, State *state) {
  char *pix_format;
  switch (state->codec_ctx->pix_fmt) {
  case AV_PIX_FMT_YUVJ420P:
  case AV_PIX_FMT_YUV420P:
    pix_format = "I420";
    break;
  case AV_PIX_FMT_YUVJ422P:
  case AV_PIX_FMT_YUV422P:
    pix_format = "I422";
    break;
  default:
    return get_metadata_result_error_pix_fmt(env);
  }
  return get_metadata_result_ok(env, state->codec_ctx->width,
                                state->codec_ctx->height, pix_format);
}

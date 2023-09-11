#include "encoder.h"

void handle_destroy_state(UnifexEnv *env, State *state) {
  UNIFEX_UNUSED(env);

  if (state->codec_ctx != NULL) {
    avcodec_free_context(&state->codec_ctx);
  }
}

UNIFEX_TERM create(UnifexEnv *env, int width, int height, char *pix_fmt,
                   char *preset, char *tune, char *profile, int max_b_frames, int gop_size,
                   int framerate_num, int framerate_denom, int crf) {
  UNIFEX_TERM res;
  AVDictionary *params = NULL;
  State *state = unifex_alloc_state(env);
  state->codec_ctx = NULL;
  state->last_pts = -1;

  // TODO: Consider using av_log_set_callback to pass messages to membrane
  // logger
  av_log_set_level(AV_LOG_QUIET);

#if (LIBAVCODEC_VERSION_MAJOR < 58)
  avcodec_register_all();
#endif
  const AVCodec *codec = avcodec_find_encoder(AV_CODEC_ID_H264);
  if (!codec) {
    res = create_result_error(env, "nocodec");
    goto exit_create;
  }

  state->codec_ctx = avcodec_alloc_context3(codec);
  if (!state->codec_ctx) {
    res = create_result_error(env, "codec_alloc");
    goto exit_create;
  }

  state->codec_ctx->width = width;
  state->codec_ctx->height = height;

  if (strcmp(pix_fmt, "I420") == 0) {
    state->codec_ctx->pix_fmt = AV_PIX_FMT_YUV420P;
  } else if (strcmp(pix_fmt, "I422") == 0) {
    state->codec_ctx->pix_fmt = AV_PIX_FMT_YUV422P;
  } else {
    res = create_result_error(env, "pix_fmt");
    goto exit_create;
  }

  state->codec_ctx->framerate.num = framerate_num;
  state->codec_ctx->framerate.den = framerate_denom;

  if (framerate_num == 0) {
    state->codec_ctx->time_base.num = 1;
    state->codec_ctx->time_base.den = 30;
  } else {
    state->codec_ctx->time_base.num = framerate_denom;
    state->codec_ctx->time_base.den = framerate_num;
  }
  if (max_b_frames > -1) {
    state->codec_ctx->max_b_frames = max_b_frames;
  }
  if (gop_size > -1) {
    state->codec_ctx->gop_size = gop_size;
  }

  av_dict_set(&params, "preset", preset, 0);

  if (strcmp("nil", profile) != 0) {
    av_dict_set(&params, "profile", profile, 0);
  }

  if (strcmp("nil", tune) != 0) {
    av_dict_set(&params, "tune", tune, 0);
  }

  av_dict_set_int(&params, "crf", crf, 0);

  if (avcodec_open2(state->codec_ctx, codec, &params) < 0) {
    res = create_result_error(env, "codec_open");
    goto exit_create;
  }

  res = create_result_ok(env, state);
exit_create:
  unifex_release_state(env, state);
  return res;
}

UNIFEX_TERM get_frame_size(UnifexEnv *env, State *state) {
  int frame_size = av_image_get_buffer_size(state->codec_ctx->pix_fmt,
                                            state->codec_ctx->width,
                                            state->codec_ctx->height, 1);

  if (frame_size < 0) {
    return get_frame_size_result_error(env);
  }

  return get_frame_size_result_ok(env, frame_size);
}

static int get_frames(UnifexEnv *env, AVFrame *frame,
                      UnifexPayload ***ret_frames, int64_t **dts_list,
                      int64_t **pts_list, int *max_frames, int *frame_cnt,
                      int use_shm, State *state) {
  AVPacket *pkt = av_packet_alloc();
  UnifexPayload **frames = unifex_alloc((*max_frames) * sizeof(*frames));
  int64_t *decoding_ts =
      unifex_alloc((*max_frames) * sizeof(*decoding_ts));
  int64_t *presentation_ts =
      unifex_alloc((*max_frames) * sizeof(*presentation_ts));

  int ret = avcodec_send_frame(state->codec_ctx, frame);
  if (ret < 0) {
    ret = ENCODER_SEND_FRAME_ERROR;
    goto exit_get_frames;
  }

  ret = avcodec_receive_packet(state->codec_ctx, pkt);
  while (ret != AVERROR(EAGAIN) && ret != AVERROR_EOF) {
    if (ret < 0) {
      ret = ENCODER_ENCODE_ERROR;
      goto exit_get_frames;
    }

    if (*frame_cnt >= (*max_frames)) {
      *max_frames *= 2;
      frames = unifex_realloc(frames, (*max_frames) * sizeof(*frames));
      decoding_ts = unifex_realloc(decoding_ts,
                                      (*max_frames) * sizeof(*decoding_ts));
      presentation_ts = unifex_realloc(presentation_ts,
                                      (*max_frames) * sizeof(*presentation_ts));
    }

    decoding_ts[*frame_cnt] = pkt->dts;
    presentation_ts[*frame_cnt] = pkt->pts;
    frames[*frame_cnt] = unifex_alloc(sizeof(UnifexPayload));
    UnifexPayloadType payload_type;
    if (use_shm) {
      payload_type = UNIFEX_PAYLOAD_SHM;
    } else {
      payload_type = UNIFEX_PAYLOAD_BINARY;
    }
    unifex_payload_alloc(env, payload_type, pkt->size, frames[*frame_cnt]);
    memcpy(frames[*frame_cnt]->data, pkt->data, pkt->size);
    (*frame_cnt)++;

    ret = avcodec_receive_packet(state->codec_ctx, pkt);
  }
  ret = 0;
exit_get_frames:
  *ret_frames = frames;
  *dts_list = decoding_ts;
  *pts_list = presentation_ts;
  av_packet_free(&pkt);
  return ret;
}

UNIFEX_TERM encode(UnifexEnv *env, UnifexPayload *payload, int64_t pts,
                   int use_shm, State *state) {
  UNIFEX_TERM res_term;
  int res = 0;
  int max_frames = 16, frame_cnt = 0;
  UnifexPayload **out_frames = NULL;
  int64_t *dts_list = NULL;
  int64_t *pts_list = NULL;

  AVFrame *frame = av_frame_alloc();
  frame->format = state->codec_ctx->pix_fmt;
  frame->width = state->codec_ctx->width;
  frame->height = state->codec_ctx->height;
  av_image_fill_arrays(frame->data, frame->linesize, payload->data,
                       frame->format, frame->width, frame->height, 1);

  if (pts == AV_NOPTS_VALUE) {
    frame->pts = state->last_pts + 1;
  } else {
    frame->pts = pts;
  }
  state->last_pts = frame->pts;

  res = get_frames(env, frame, &out_frames, &dts_list, &pts_list, &max_frames,
                   &frame_cnt, use_shm, state);

  switch (res) {
  case ENCODER_SEND_FRAME_ERROR:
    res_term = encode_result_error(env, "send_frame");
    break;
  case ENCODER_ENCODE_ERROR:
    res_term = encode_result_error(env, "encode");
    break;
  default:
    res_term = encode_result_ok(env, dts_list, frame_cnt, pts_list, frame_cnt,
                                out_frames, frame_cnt);
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
  if (dts_list != NULL) {
    unifex_free(dts_list);
  }
  av_frame_free(&frame);
  return res_term;
}

UNIFEX_TERM flush(UnifexEnv *env, int use_shm, State *state) {
  UNIFEX_TERM res_term;
  int max_frames = 16, frame_cnt = 0;
  UnifexPayload **out_frames = NULL;
  int64_t *dts_list = NULL;
  int64_t *pts_list = NULL;

  int res = get_frames(env, NULL, &out_frames, &dts_list, &pts_list,
                       &max_frames, &frame_cnt, use_shm, state);
  switch (res) {
  case ENCODER_SEND_FRAME_ERROR:
    res_term = encode_result_error(env, "send_frame");
    break;
  case ENCODER_ENCODE_ERROR:
    res_term = encode_result_error(env, "encode");
    break;
  default:
    res_term = encode_result_ok(env, dts_list, frame_cnt, pts_list, frame_cnt,
                                out_frames, frame_cnt);
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
  if (dts_list != NULL) {
    unifex_free(dts_list);
  }
  return res_term;
}

#pragma once

#include <errno.h>
#include <stdarg.h>
#include <string.h>
#ifdef __GNUC__
#include <stdint.h>
#endif

// format encoding constants
#define MEMBRANE_SAMPLE_FORMAT_TYPE ((uint32_t)(0b11 << 30))
#define MEMBRANE_SAMPLE_FORMAT_TYPE_SIGN ((uint32_t)(0b1 << 30))
#define MEMBRANE_SAMPLE_FORMAT_ENDIANITY ((uint32_t)(0b1 << 29))
#define MEMBRANE_SAMPLE_FORMAT_SIZE ((uint32_t)((0b1 << 8) - 1))
#define MEMBRANE_SAMPLE_FORMAT_TYPE_U ((uint32_t)(0b00 << 30))
#define MEMBRANE_SAMPLE_FORMAT_TYPE_S ((uint32_t)(0b01 << 30))
#define MEMBRANE_SAMPLE_FORMAT_TYPE_F ((uint32_t)(0b11 << 30))
#define MEMBRANE_SAMPLE_FORMAT_ENDIANITY_LE ((uint32_t)(0b0 << 29))
#define MEMBRANE_SAMPLE_FORMAT_ENDIANITY_BE ((uint32_t)(0b1 << 29))

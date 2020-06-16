////////////////////////////////////////////////////////////////////////////
//                           **** WAVPACK ****                            //
//                  Hybrid Lossless Wavefile Compressor                   //
//              Copyright (c) 1998 - 2006 Conifer Software.               //
//                          All Rights Reserved.                          //
//      Distributed under the BSD Software License (see license.txt)      //
////////////////////////////////////////////////////////////////////////////

// wavpack_local.h

#ifndef WAVPACK_LOCAL_H
#define WAVPACK_LOCAL_H

//#if defined(WIN32)
//#define FASTCALL __fastcall
//#else
#define FASTCALL
//#endif

#if defined(WIN32) || \
    (defined(BYTE_ORDER) && defined(LITTLE_ENDIAN) && (BYTE_ORDER == LITTLE_ENDIAN))
#define BITSTREAM_SHORTS    // use "shorts" for reading/writing bitstreams
                            //  (only works on little-endian machines)
#endif

#include <sys/types.h>

// This header file contains all the definitions required by WavPack.

#if defined(_WIN32) && !defined(__MINGW32__)
#include <stdlib.h>
typedef unsigned __int64 uint64_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int16 uint16_t;
typedef unsigned __int8 uint8_t;
typedef __int64 int64_t;
typedef __int32 int32_t;
typedef __int16 int16_t;
typedef __int8  int8_t;
typedef float float32_t;
#else
#include <inttypes.h>
#endif

// Because the C99 specification states that "The order of allocation of
// bit-ﬁelds within a unit (high-order to low-order or low-order to
// high-order) is implementation-deﬁned" (6.7.2.1), I decided to change
// the representation of floating-point values from a structure of
// bit-fields to a 32-bit integer with access macros. Note that the WavPack
// library doesn't use any floating-point math to implement compression of
// floating-point data (although a little floating-point math is used in
// high-level functions unrelated to the codec).

typedef int32_t f32;

#define get_mantissa(f)     ((f) & 0x7fffff)
#define get_exponent(f)     (((f) >> 23) & 0xff)
#define get_sign(f)         (((f) >> 31) & 0x1)

#define set_mantissa(f,v)   (f) ^= (((f) ^ (v)) & 0x7fffff)
#define set_exponent(f,v)   (f) ^= (((f) ^ ((v) << 23)) & 0x7f800000)
#define set_sign(f,v)       (f) ^= (((f) ^ ((v) << 31)) & 0x80000000)

#include <stdio.h>

#define FALSE 0
#define TRUE 1

// ID3v1 and APEv2 TAG formats (may occur at the end of WavPack files)

typedef struct {
    char tag_id [3], title [30], artist [30], album [30];
    char year [4], comment [30], genre;
} ID3_Tag;

typedef struct {
    char ID [8];
    int32_t version, length, item_count, flags;
    char res [8];
} APE_Tag_Hdr;

#define APE_Tag_Hdr_Format "8LLLL"

#define APE_TAG_TYPE_TEXT       0x0
#define APE_TAG_TYPE_BINARY     0x1
#define APE_TAG_THIS_IS_HEADER  0x20000000
#define APE_TAG_CONTAINS_HEADER 0x80000000
#define APE_TAG_MAX_LENGTH      (1024 * 1024)

typedef struct {
    int32_t tag_file_pos;
    ID3_Tag id3_tag;
    APE_Tag_Hdr ape_tag_hdr;
    unsigned char *ape_tag_data;
} M_Tag;

// RIFF / wav header formats (these occur at the beginning of both wav files
// and pre-4.0 WavPack files that are not in the "raw" mode)

typedef struct {
    char ckID [4];
    uint32_t ckSize;
    char formType [4];
} RiffChunkHeader;

typedef struct {
    char ckID [4];
    uint32_t ckSize;
} ChunkHeader;

#define ChunkHeaderFormat "4L"

typedef struct {
    unsigned short FormatTag, NumChannels;
    uint32_t SampleRate, BytesPerSecond;
    unsigned short BlockAlign, BitsPerSample;
    unsigned short cbSize, ValidBitsPerSample;
    int32_t ChannelMask;
    unsigned short SubFormat;
    char GUID [14];
} WaveHeader;

#define WaveHeaderFormat "SSLLSSSSLS"

////////////////////////////// WavPack Header /////////////////////////////////

// Note that this is the ONLY structure that is written to (or read from)
// WavPack 4.0 files, and is the preamble to every block in both the .wv
// and .wvc files.

typedef struct {
    char ckID [4];
    uint32_t ckSize;
    short version;
    unsigned char track_no, index_no;
    uint32_t total_samples, block_index, block_samples, flags, crc;
} WavpackHeader;

#define WavpackHeaderFormat "4LS2LLLLL"

// or-values for "flags"

#define BYTES_STORED    3       // 1-4 bytes/sample
#define MONO_FLAG       4       // not stereo
#define HYBRID_FLAG     8       // hybrid mode
#define JOINT_STEREO    0x10    // joint stereo
#define CROSS_DECORR    0x20    // no-delay cross decorrelation
#define HYBRID_SHAPE    0x40    // noise shape (hybrid mode only)
#define FLOAT_DATA      0x80    // ieee 32-bit floating point data

#define INT32_DATA      0x100   // special extended int handling
#define HYBRID_BITRATE  0x200   // bitrate noise (hybrid mode only)
#define HYBRID_BALANCE  0x400   // balance noise (hybrid stereo mode only)

#define INITIAL_BLOCK   0x800   // initial block of multichannel segment
#define FINAL_BLOCK     0x1000  // final block of multichannel segment

#define SHIFT_LSB       13
#define SHIFT_MASK      (0x1fL << SHIFT_LSB)

#define MAG_LSB         18
#define MAG_MASK        (0x1fL << MAG_LSB)

#define SRATE_LSB       23
#define SRATE_MASK      (0xfL << SRATE_LSB)

#define FALSE_STEREO    0x40000000      // block is stereo, but data is mono

#define IGNORED_FLAGS   0x18000000      // reserved, but ignore if encountered
#define NEW_SHAPING     0x20000000      // use IIR filter for negative shaping
#define UNKNOWN_FLAGS   0x80000000      // also reserved, but refuse decode if
                                        //  encountered

#define MONO_DATA (MONO_FLAG | FALSE_STEREO)

#define MIN_STREAM_VERS     0x402       // lowest stream version we'll decode
#define MAX_STREAM_VERS     0x410       // highest stream version we'll decode or encode
#define CUR_STREAM_VERS     0x407       // stream version we are [normally] writing now


//////////////////////////// WavPack Metadata /////////////////////////////////

// This is an internal representation of metadata.

typedef struct {
    int32_t byte_length;
    void *data;
    unsigned char id;
} WavpackMetadata;

#define ID_UNIQUE               0x3f
#define ID_OPTIONAL_DATA        0x20
#define ID_ODD_SIZE             0x40
#define ID_LARGE                0x80

#define ID_DUMMY                0x0
#define ID_ENCODER_INFO         0x1
#define ID_DECORR_TERMS         0x2
#define ID_DECORR_WEIGHTS       0x3
#define ID_DECORR_SAMPLES       0x4
#define ID_ENTROPY_VARS         0x5
#define ID_HYBRID_PROFILE       0x6
#define ID_SHAPING_WEIGHTS      0x7
#define ID_FLOAT_INFO           0x8
#define ID_INT32_INFO           0x9
#define ID_WV_BITSTREAM         0xa
#define ID_WVC_BITSTREAM        0xb
#define ID_WVX_BITSTREAM        0xc
#define ID_CHANNEL_INFO         0xd

#define ID_RIFF_HEADER          (ID_OPTIONAL_DATA | 0x1)
#define ID_RIFF_TRAILER         (ID_OPTIONAL_DATA | 0x2)
#define ID_REPLAY_GAIN          (ID_OPTIONAL_DATA | 0x3)
#define ID_CUESHEET             (ID_OPTIONAL_DATA | 0x4)
#define ID_CONFIG_BLOCK         (ID_OPTIONAL_DATA | 0x5)
#define ID_MD5_CHECKSUM         (ID_OPTIONAL_DATA | 0x6)
#define ID_SAMPLE_RATE          (ID_OPTIONAL_DATA | 0x7)

///////////////////////// WavPack Configuration ///////////////////////////////

// This internal structure is used during encode to provide configuration to
// the encoding engine and during decoding to provide fle information back to
// the higher level functions. Not all fields are used in both modes.

typedef struct {
    float bitrate, shaping_weight;
    int bits_per_sample, bytes_per_sample;
    int qmode, flags, xmode, num_channels, float_norm_exp;
    int32_t block_samples, extra_flags, sample_rate, channel_mask;
    unsigned char md5_checksum [16], md5_read;
    int num_tag_strings;
    char **tag_strings;
} WavpackConfig;

#define CONFIG_BYTES_STORED     3       // 1-4 bytes/sample
#define CONFIG_MONO_FLAG        4       // not stereo
#define CONFIG_HYBRID_FLAG      8       // hybrid mode
#define CONFIG_JOINT_STEREO     0x10    // joint stereo
#define CONFIG_CROSS_DECORR     0x20    // no-delay cross decorrelation
#define CONFIG_HYBRID_SHAPE     0x40    // noise shape (hybrid mode only)
#define CONFIG_FLOAT_DATA       0x80    // ieee 32-bit floating point data

#define CONFIG_FAST_FLAG        0x200   // fast mode
#define CONFIG_HIGH_FLAG        0x800   // high quality mode
#define CONFIG_VERY_HIGH_FLAG   0x1000  // very high
#define CONFIG_BITRATE_KBPS     0x2000  // bitrate is kbps, not bits / sample
#define CONFIG_AUTO_SHAPING     0x4000  // automatic noise shaping
#define CONFIG_SHAPE_OVERRIDE   0x8000  // shaping mode specified
#define CONFIG_JOINT_OVERRIDE   0x10000 // joint-stereo mode specified
#define CONFIG_DYNAMIC_SHAPING  0x20000 // dynamic noise shaping
#define CONFIG_CREATE_EXE       0x40000 // create executable
#define CONFIG_CREATE_WVC       0x80000 // create correction file
#define CONFIG_OPTIMIZE_WVC     0x100000 // maximize bybrid compression
#define CONFIG_CALC_NOISE       0x800000 // calc noise in hybrid mode
#define CONFIG_LOSSY_MODE       0x1000000 // obsolete (for information)
#define CONFIG_EXTRA_MODE       0x2000000 // extra processing mode
#define CONFIG_SKIP_WVX         0x4000000 // no wvx stream w/ floats & big ints
#define CONFIG_MD5_CHECKSUM     0x8000000 // compute & store MD5 signature
#define CONFIG_MERGE_BLOCKS     0x10000000 // merge blocks of equal redundancy (for lossyWAV)
#define CONFIG_PAIR_UNDEF_CHANS 0x20000000 // encode undefined channels in stereo pairs
#define CONFIG_OPTIMIZE_MONO    0x80000000 // optimize for mono streams posing as stereo

/*
 * These config flags were never actually used, or are no longer used, or are
 * used for something else now. They may be used in the future for what they
 * say, or for something else. WavPack files in the wild *may* have some of
 * these bit set in their config flags (with these older meanings), but only
 * if the stream version is 0x410 or less than 0x407. Of course, this is not
 * very important because once the file has been encoded, the config bits are
 * just for information purposes (i.e., they do not affect decoding),
 *
#define CONFIG_ADOBE_MODE       0x100   // "adobe" mode for 32-bit floats
#define CONFIG_VERY_FAST_FLAG   0x400   // double fast
#define CONFIG_COPY_TIME        0x20000 // copy file-time from source
#define CONFIG_QUALITY_MODE     0x200000 // psychoacoustic quality mode
#define CONFIG_RAW_FLAG         0x400000 // raw mode (not implemented yet)
#define CONFIG_QUIET_MODE       0x10000000 // don't report progress %
#define CONFIG_IGNORE_LENGTH    0x20000000 // ignore length in wav header
#define CONFIG_NEW_RIFF_HEADER  0x40000000 // generate new RIFF wav header
 *
 */

#define EXTRA_SCAN_ONLY         1
#define EXTRA_STEREO_MODES      2
#define EXTRA_TRY_DELTAS        8
#define EXTRA_ADJUST_DELTAS     16
#define EXTRA_SORT_FIRST        32
#define EXTRA_BRANCHES          0x1c0
#define EXTRA_SKIP_8TO16        512
#define EXTRA_TERMS             0x3c00
#define EXTRA_DUMP_TERMS        16384
#define EXTRA_SORT_LAST         32768

//////////////////////////////// WavPack Stream ///////////////////////////////

// This internal structure contains everything required to handle a WavPack
// "stream", which is defined as a stereo or mono stream of audio samples. For
// multichannel audio several of these would be required. Each stream contains
// pointers to hold a complete allocated block of WavPack data, although it's
// possible to decode WavPack blocks without buffering an entire block.

typedef struct bs {
#ifdef BITSTREAM_SHORTS
    unsigned short *buf, *end, *ptr;
#else
    unsigned char *buf, *end, *ptr;
#endif
    void (*wrap)(struct bs *bs);
    int error, bc;
    uint32_t sr;
} Bitstream;

#define MAX_WRAPPER_BYTES 16777216
#define NEW_MAX_STREAMS 4096
#define OLD_MAX_STREAMS 8
#define MAX_NTERMS 16
#define MAX_TERM 8

struct decorr_pass {
    int term, delta, weight_A, weight_B;
    int32_t samples_A [MAX_TERM], samples_B [MAX_TERM];
    int32_t aweight_A, aweight_B;
    int32_t sum_A, sum_B;
};

typedef struct {
    char joint_stereo, delta, terms [MAX_NTERMS+1];
} WavpackDecorrSpec;

struct entropy_data {
    uint32_t median [3], slow_level, error_limit;
};

struct words_data {
    uint32_t bitrate_delta [2], bitrate_acc [2];
    uint32_t pend_data, holding_one, zeros_acc;
    int holding_zero, pend_count;
    struct entropy_data c [2];
};

typedef struct {
    WavpackHeader wphdr;
    struct words_data w;

    unsigned char *blockbuff, *blockend;
    unsigned char *block2buff, *block2end;
    int32_t *sample_buffer;

    int bits, num_terms, mute_error, joint_stereo, false_stereo, shift;
    int num_decorrs, num_passes, best_decorr, mask_decorr;
    uint32_t sample_index, crc, crc_x, crc_wvx;
    Bitstream wvbits, wvcbits, wvxbits;
    int init_done, wvc_skip;
    float delta_decay;

    unsigned char int32_sent_bits, int32_zeros, int32_ones, int32_dups;
    unsigned char float_flags, float_shift, float_max_exp, float_norm_exp;

    struct {
        int32_t shaping_acc [2], shaping_delta [2], error [2];
        double noise_sum, noise_ave, noise_max;
        short *shaping_data, *shaping_array;
        int32_t shaping_samples;
    } dc;

    struct decorr_pass decorr_passes [MAX_NTERMS], analysis_pass;
    const WavpackDecorrSpec *decorr_specs;
} WavpackStream;

// flags for float_flags:

#define FLOAT_SHIFT_ONES 1      // bits left-shifted into float = '1'
#define FLOAT_SHIFT_SAME 2      // bits left-shifted into float are the same
#define FLOAT_SHIFT_SENT 4      // bits shifted into float are sent literally
#define FLOAT_ZEROS_SENT 8      // "zeros" are not all real zeros
#define FLOAT_NEG_ZEROS  0x10   // contains negative zeros
#define FLOAT_EXCEPTIONS 0x20   // contains exceptions (inf, nan, etc.)

/////////////////////////////// WavPack Context ///////////////////////////////

// This internal structure holds everything required to encode or decode WavPack
// files. It is recommended that direct access to this structure be minimized
// and the provided utilities used instead.

typedef struct {
    int32_t (*read_bytes)(void *id, void *data, int32_t bcount);
    uint32_t (*get_pos)(void *id);
    int (*set_pos_abs)(void *id, uint32_t pos);
    int (*set_pos_rel)(void *id, int32_t delta, int mode);
    int (*push_back_byte)(void *id, int c);
    uint32_t (*get_length)(void *id);
    int (*can_seek)(void *id);

    // this callback is for writing edited tags only
    int32_t (*write_bytes)(void *id, void *data, int32_t bcount);
} WavpackStreamReader;

typedef int (*WavpackBlockOutput)(void *id, void *data, int32_t bcount);

typedef struct {
    WavpackConfig config;

    WavpackMetadata *metadata;
    uint32_t metabytes;
    int metacount;

    unsigned char *wrapper_data;
    uint32_t wrapper_bytes;

    WavpackBlockOutput blockout;
    void *wv_out, *wvc_out;

    WavpackStreamReader *reader;
    void *wv_in, *wvc_in;

    uint32_t filelen, file2len, filepos, file2pos, total_samples, crc_errors, first_flags;
    int wvc_flag, open_flags, norm_offset, reduced_channels, lossy_blocks, close_files;
    uint32_t block_samples, ave_block_samples, block_boundary, max_samples, acc_samples, initial_index, riff_trailer_bytes;
    int riff_header_added, riff_header_created;
    M_Tag m_tag;

    int current_stream, num_streams, max_streams, stream_version;
    WavpackStream **streams;
    void *stream3;

    char error_message [80];
} WavpackContext;

//////////////////////// function prototypes and macros //////////////////////

#define CLEAR(destin) memset (&destin, 0, sizeof (destin));

// These macros implement the weight application and update operations
// that are at the heart of the decorrelation loops. Note that there are
// sometimes two and even three versions of each macro. Theses should be
// equivalent and produce identical results, but some may perform better
// or worse on a given architecture.

#if 1   // PERFCOND - apply decorrelation weight when no 32-bit overflow possible
#define apply_weight_i(weight, sample) ((weight * sample + 512) >> 10)
#else
#define apply_weight_i(weight, sample) ((((weight * sample) >> 8) + 2) >> 2)
#endif

#if 1   // PERFCOND - apply decorrelation weight when 32-bit overflow is possible
#define apply_weight_f(weight, sample) (((((sample & 0xffff) * weight) >> 9) + \
    (((sample & ~0xffff) >> 9) * weight) + 1) >> 1)
#else
#define apply_weight_f(weight, sample) ((int32_t)floor(((double) weight * sample + 512.0) / 1024.0))
#endif

#if 1   // PERFCOND - universal version that checks input magnitude (or simply uses 64-bit ints)
#define apply_weight(weight, sample) (sample != (short) sample ? \
    apply_weight_f (weight, sample) : apply_weight_i (weight, sample))
#else
#define apply_weight(weight, sample) ((int32_t)((weight * (int64_t) sample + 512) >> 10))
#endif

#if 1   // PERFCOND
#define update_weight(weight, delta, source, result) \
    if (source && result) { int32_t s = (int32_t) (source ^ result) >> 31; weight = (delta ^ s) + (weight - s); }
#elif 1
#define update_weight(weight, delta, source, result) \
    if (source && result) weight += (((source ^ result) >> 30) | 1) * delta;
#else
#define update_weight(weight, delta, source, result) \
    if (source && result) (source ^ result) < 0 ? (weight -= delta) : (weight += delta);
#endif

#define update_weight_d2(weight, delta, source, result) \
    if (source && result) weight -= (((source ^ result) >> 29) & 4) - 2;

#define update_weight_clip(weight, delta, source, result) \
    if (source && result) { \
        const int32_t s = (source ^ result) >> 31; \
        if ((weight = (weight ^ s) + (delta - s)) > 1024) weight = 1024; \
        weight = (weight ^ s) - s; \
    }

#define update_weight_clip_d2(weight, delta, source, result) \
    if (source && result) { \
        const int32_t s = (source ^ result) >> 31; \
        if ((weight = (weight ^ s) + (2 - s)) > 1024) weight = 1024; \
        weight = (weight ^ s) - s; \
    }

// bits.c

void bs_open_read (Bitstream *bs, void *buffer_start, void *buffer_end);
void bs_open_write (Bitstream *bs, void *buffer_start, void *buffer_end);
uint32_t bs_close_read (Bitstream *bs);
uint32_t bs_close_write (Bitstream *bs);

int DoReadFile (FILE *hFile, void *lpBuffer, uint32_t nNumberOfBytesToRead, uint32_t *lpNumberOfBytesRead);
int DoWriteFile (FILE *hFile, void *lpBuffer, uint32_t nNumberOfBytesToWrite, uint32_t *lpNumberOfBytesWritten);
uint32_t DoGetFileSize (FILE *hFile), DoGetFilePosition (FILE *hFile);
int DoSetFilePositionRelative (FILE *hFile, int32_t pos, int mode);
int DoSetFilePositionAbsolute (FILE *hFile, uint32_t pos);
int DoUngetc (int c, FILE *hFile), DoDeleteFile (char *filename);
int DoCloseHandle (FILE *hFile), DoTruncateFile (FILE *hFile);

#define bs_is_open(bs) ((bs)->ptr != NULL)

#define getbit(bs) ( \
    (((bs)->bc) ? \
        ((bs)->bc--, (bs)->sr & 1) : \
            (((++((bs)->ptr) != (bs)->end) ? (void) 0 : (bs)->wrap (bs)), (bs)->bc = sizeof (*((bs)->ptr)) * 8 - 1, ((bs)->sr = *((bs)->ptr)) & 1) \
    ) ? \
        ((bs)->sr >>= 1, 1) : \
        ((bs)->sr >>= 1, 0) \
)

#define getbits(value, nbits, bs) { \
    while ((nbits) > (bs)->bc) { \
        if (++((bs)->ptr) == (bs)->end) (bs)->wrap (bs); \
        (bs)->sr |= (int32_t)*((bs)->ptr) << (bs)->bc; \
        (bs)->bc += sizeof (*((bs)->ptr)) * 8; \
    } \
    *(value) = (bs)->sr; \
    if ((bs)->bc > 32) { \
        (bs)->bc -= (nbits); \
        (bs)->sr = *((bs)->ptr) >> (sizeof (*((bs)->ptr)) * 8 - (bs)->bc); \
    } \
    else { \
        (bs)->bc -= (nbits); \
        (bs)->sr >>= (nbits); \
    } \
}

#define putbit(bit, bs) { if (bit) (bs)->sr |= (1 << (bs)->bc); \
    if (++((bs)->bc) == sizeof (*((bs)->ptr)) * 8) { \
        *((bs)->ptr) = (bs)->sr; \
        (bs)->sr = (bs)->bc = 0; \
        if (++((bs)->ptr) == (bs)->end) (bs)->wrap (bs); \
    }}

#define putbit_0(bs) { \
    if (++((bs)->bc) == sizeof (*((bs)->ptr)) * 8) { \
        *((bs)->ptr) = (bs)->sr; \
        (bs)->sr = (bs)->bc = 0; \
        if (++((bs)->ptr) == (bs)->end) (bs)->wrap (bs); \
    }}

#define putbit_1(bs) { (bs)->sr |= (1 << (bs)->bc); \
    if (++((bs)->bc) == sizeof (*((bs)->ptr)) * 8) { \
        *((bs)->ptr) = (bs)->sr; \
        (bs)->sr = (bs)->bc = 0; \
        if (++((bs)->ptr) == (bs)->end) (bs)->wrap (bs); \
    }}

#define putbits(value, nbits, bs) { \
    (bs)->sr |= (int32_t)(value) << (bs)->bc; \
    if (((bs)->bc += (nbits)) >= sizeof (*((bs)->ptr)) * 8) \
        do { \
            *((bs)->ptr) = (bs)->sr; \
            (bs)->sr >>= sizeof (*((bs)->ptr)) * 8; \
            if (((bs)->bc -= sizeof (*((bs)->ptr)) * 8) > 32 - sizeof (*((bs)->ptr)) * 8) \
                (bs)->sr |= ((value) >> ((nbits) - (bs)->bc)); \
            if (++((bs)->ptr) == (bs)->end) (bs)->wrap (bs); \
        } while ((bs)->bc >= sizeof (*((bs)->ptr)) * 8); \
}

void little_endian_to_native (void *data, char *format);
void native_to_little_endian (void *data, char *format);

// pack.c

void pack_init (WavpackContext *wpc);
int pack_block (WavpackContext *wpc, int32_t *buffer);
double WavpackGetEncodedNoise (WavpackContext *wpc, double *peak);

// unpack.c

int unpack_init (WavpackContext *wpc);
int init_wv_bitstream (WavpackStream *wps, WavpackMetadata *wpmd);
int init_wvc_bitstream (WavpackStream *wps, WavpackMetadata *wpmd);
int init_wvx_bitstream (WavpackStream *wps, WavpackMetadata *wpmd);
int read_decorr_terms (WavpackStream *wps, WavpackMetadata *wpmd);
int read_decorr_weights (WavpackStream *wps, WavpackMetadata *wpmd);
int read_decorr_samples (WavpackStream *wps, WavpackMetadata *wpmd);
int read_shaping_info (WavpackStream *wps, WavpackMetadata *wpmd);
int read_float_info (WavpackStream *wps, WavpackMetadata *wpmd);
int read_int32_info (WavpackStream *wps, WavpackMetadata *wpmd);
int read_channel_info (WavpackContext *wpc, WavpackMetadata *wpmd);
int read_config_info (WavpackContext *wpc, WavpackMetadata *wpmd);
int read_sample_rate (WavpackContext *wpc, WavpackMetadata *wpmd);
int read_wrapper_data (WavpackContext *wpc, WavpackMetadata *wpmd);
int32_t unpack_samples (WavpackContext *wpc, int32_t *buffer, uint32_t sample_count);
int check_crc_error (WavpackContext *wpc);

// unpack3.c

WavpackContext *open_file3 (WavpackContext *wpc, char *error);
int32_t unpack_samples3 (WavpackContext *wpc, int32_t *buffer, uint32_t sample_count);
int seek_sample3 (WavpackContext *wpc, uint32_t desired_index);
uint32_t get_sample_index3 (WavpackContext *wpc);
void free_stream3 (WavpackContext *wpc);
int get_version3 (WavpackContext *wpc);

// metadata.c stuff

int read_metadata_buff (WavpackMetadata *wpmd, unsigned char *blockbuff, unsigned char **buffptr);
int write_metadata_block (WavpackContext *wpc);
int copy_metadata (WavpackMetadata *wpmd, unsigned char *buffer_start, unsigned char *buffer_end);
int add_to_metadata (WavpackContext *wpc, void *data, uint32_t bcount, unsigned char id);
int process_metadata (WavpackContext *wpc, WavpackMetadata *wpmd);
void free_metadata (WavpackMetadata *wpmd);

// words.c stuff

void init_words (WavpackStream *wps);
void word_set_bitrate (WavpackStream *wps);
void write_entropy_vars (WavpackStream *wps, WavpackMetadata *wpmd);
void write_hybrid_profile (WavpackStream *wps, WavpackMetadata *wpmd);
int read_entropy_vars (WavpackStream *wps, WavpackMetadata *wpmd);
int read_hybrid_profile (WavpackStream *wps, WavpackMetadata *wpmd);
int32_t FASTCALL send_word (WavpackStream *wps, int32_t value, int chan);
void send_words_lossless (WavpackStream *wps, int32_t *buffer, int32_t nsamples);
int32_t FASTCALL get_word (WavpackStream *wps, int chan, int32_t *correction);
int32_t get_words_lossless (WavpackStream *wps, int32_t *buffer, int32_t nsamples);
void flush_word (WavpackStream *wps);
int32_t nosend_word (WavpackStream *wps, int32_t value, int chan);
void scan_word (WavpackStream *wps, int32_t *samples, uint32_t num_samples, int dir);

int log2s (int32_t value);
int32_t exp2s (int log);
uint32_t log2buffer (int32_t *samples, uint32_t num_samples, int limit);

signed char store_weight (int weight);
int restore_weight (signed char weight);

#define WORD_EOF ((int32_t)(1L << 31))

// float.c

void write_float_info (WavpackStream *wps, WavpackMetadata *wpmd);
int scan_float_data (WavpackStream *wps, f32 *values, int32_t num_values);
void send_float_data (WavpackStream *wps, f32 *values, int32_t num_values);
int read_float_info (WavpackStream *wps, WavpackMetadata *wpmd);
void float_values (WavpackStream *wps, int32_t *values, int32_t num_values);
void WavpackFloatNormalize (int32_t *values, int32_t num_values, int delta_exp);

// extra?.c

// void analyze_stereo (WavpackContext *wpc, int32_t *samples);
// void analyze_mono (WavpackContext *wpc, int32_t *samples);
void execute_stereo (WavpackContext *wpc, int32_t *samples, int no_history, int do_samples);
void execute_mono (WavpackContext *wpc, int32_t *samples, int no_history, int do_samples);

// wputils.c

WavpackContext *WavpackOpenFileInputEx (WavpackStreamReader *reader, void *wv_id, void *wvc_id, char *error, int flags, int norm_offset);
WavpackContext *WavpackOpenFileInput (const char *infilename, char *error, int flags, int norm_offset);

#define OPEN_WVC        0x1     // open/read "correction" file
#define OPEN_TAGS       0x2     // read ID3v1 / APEv2 tags (seekable file)
#define OPEN_WRAPPER    0x4     // make audio wrapper available (i.e. RIFF)
#define OPEN_2CH_MAX    0x8     // open multichannel as stereo (no downmix)
#define OPEN_NORMALIZE  0x10    // normalize floating point data to +/- 1.0
#define OPEN_STREAMING  0x20    // "streaming" mode blindly unpacks blocks
                                // w/o regard to header file position info
#define OPEN_EDIT_TAGS  0x40    // allow editing of tags

int WavpackGetMode (WavpackContext *wpc);

#define MODE_WVC        0x1
#define MODE_LOSSLESS   0x2
#define MODE_HYBRID     0x4
#define MODE_FLOAT      0x8
#define MODE_VALID_TAG  0x10
#define MODE_HIGH       0x20
#define MODE_FAST       0x40
#define MODE_EXTRA      0x80    // extra mode used, see MODE_XMODE for possible level
#define MODE_APETAG     0x100
#define MODE_SFX        0x200
#define MODE_VERY_HIGH  0x400
#define MODE_MD5        0x800
#define MODE_XMODE      0x7000  // mask for extra level (1-6, 0=unknown)
#define MODE_DNS        0x8000

char *WavpackGetErrorMessage (WavpackContext *wpc);
int WavpackGetVersion (WavpackContext *wpc);
uint32_t WavpackUnpackSamples (WavpackContext *wpc, int32_t *buffer, uint32_t samples);
uint32_t WavpackGetNumSamples (WavpackContext *wpc);
uint32_t WavpackGetSampleIndex (WavpackContext *wpc);
int WavpackGetNumErrors (WavpackContext *wpc);
int WavpackLossyBlocks (WavpackContext *wpc);
int WavpackSeekSample (WavpackContext *wpc, uint32_t sample);
WavpackContext *WavpackCloseFile (WavpackContext *wpc);
uint32_t WavpackGetSampleRate (WavpackContext *wpc);
int WavpackGetBitsPerSample (WavpackContext *wpc);
int WavpackGetBytesPerSample (WavpackContext *wpc);
int WavpackGetNumChannels (WavpackContext *wpc);
int WavpackGetChannelMask (WavpackContext *wpc);
int WavpackGetReducedChannels (WavpackContext *wpc);
int WavpackGetFloatNormExp (WavpackContext *wpc);
int WavpackGetMD5Sum (WavpackContext *wpc, unsigned char data [16]);
uint32_t WavpackGetWrapperBytes (WavpackContext *wpc);
unsigned char *WavpackGetWrapperData (WavpackContext *wpc);
void WavpackFreeWrapper (WavpackContext *wpc);
void WavpackSeekTrailingWrapper (WavpackContext *wpc);
double WavpackGetProgress (WavpackContext *wpc);
uint32_t WavpackGetFileSize (WavpackContext *wpc);
double WavpackGetRatio (WavpackContext *wpc);
double WavpackGetAverageBitrate (WavpackContext *wpc, int count_wvc);
double WavpackGetInstantBitrate (WavpackContext *wpc);

WavpackContext *WavpackOpenFileOutput (WavpackBlockOutput blockout, void *wv_id, void *wvc_id);
int WavpackSetConfiguration (WavpackContext *wpc, WavpackConfig *config, uint32_t total_samples);
int WavpackAddWrapper (WavpackContext *wpc, void *data, uint32_t bcount);
int WavpackStoreMD5Sum (WavpackContext *wpc, unsigned char data [16]);
int WavpackPackInit (WavpackContext *wpc);
int WavpackPackSamples (WavpackContext *wpc, int32_t *sample_buffer, uint32_t sample_count);
int WavpackFlushSamples (WavpackContext *wpc);
void WavpackUpdateNumSamples (WavpackContext *wpc, void *first_block);
void *WavpackGetWrapperLocation (void *first_block, uint32_t *size);

void WavpackLittleEndianToNative (void *data, char *format);
void WavpackNativeToLittleEndian (void *data, char *format);

uint32_t WavpackGetLibraryVersion (void);
const char *WavpackGetLibraryVersionString (void);

// tags.c

int WavpackGetNumTagItems (WavpackContext *wpc);
int WavpackGetTagItem (WavpackContext *wpc, const char *item, char *value, int size);
int WavpackGetTagItemIndexed (WavpackContext *wpc, int index, char *item, int size);
int WavpackGetNumBinaryTagItems (WavpackContext *wpc);
int WavpackGetBinaryTagItem (WavpackContext *wpc, const char *item, char *value, int size);
int WavpackGetBinaryTagItemIndexed (WavpackContext *wpc, int index, char *item, int size);
int WavpackAppendTagItem (WavpackContext *wpc, const char *item, const char *value, int vsize);
int WavpackAppendBinaryTagItem (WavpackContext *wpc, const char *item, const char *value, int vsize);
int WavpackDeleteTagItem (WavpackContext *wpc, const char *item);
int WavpackWriteTag (WavpackContext *wpc);
int load_tag (WavpackContext *wpc);
void free_tag (M_Tag *m_tag);
int valid_tag (M_Tag *m_tag);

///////////////////////////// SIMD helper macros /////////////////////////////

#ifdef OPT_MMX

#if defined (__GNUC__) && !defined (__INTEL_COMPILER)
//directly map to gcc's native builtins for faster code

#if __GNUC__ < 4
typedef int __di __attribute__ ((__mode__ (__DI__)));
typedef int __m64 __attribute__ ((__mode__ (__V2SI__)));
typedef int __v4hi __attribute__ ((__mode__ (__V4HI__)));
#define _m_paddsw(m1, m2) (__m64) __builtin_ia32_paddsw ((__v4hi) m1, (__v4hi) m2)
#define _m_pand(m1, m2) (__m64) __builtin_ia32_pand ((__di) m1, (__di) m2)
#define _m_pandn(m1, m2) (__m64) __builtin_ia32_pandn ((__di) m1, (__di) m2)
#define _m_pmaddwd(m1, m2) __builtin_ia32_pmaddwd ((__v4hi) m1, (__v4hi) m2)
#define _m_por(m1, m2) (__m64) __builtin_ia32_por ((__di) m1, (__di) m2)
#define _m_pxor(m1, m2) (__m64) __builtin_ia32_pxor ((__di) m1, (__di) m2)
#else
typedef int __m64 __attribute__ ((__vector_size__ (8)));
typedef short __m64_16 __attribute__ ((__vector_size__ (8)));
#define _m_paddsw(m1, m2) (__m64) __builtin_ia32_paddsw ((__m64_16) m1, (__m64_16) m2)
#define _m_pand(m1, m2) __builtin_ia32_pand (m1, m2)
#define _m_pandn(m1, m2) __builtin_ia32_pandn (m1, m2)
#define _m_pmaddwd(m1, m2) __builtin_ia32_pmaddwd ((__m64_16) m1, (__m64_16) m2)
#define _m_por(m1, m2) __builtin_ia32_por (m1, m2)
#define _m_pxor(m1, m2) __builtin_ia32_pxor (m1, m2)
#endif

#define _m_paddd(m1, m2) __builtin_ia32_paddd (m1, m2)
#define _m_pcmpeqd(m1, m2) __builtin_ia32_pcmpeqd (m1, m2)

#if (__GNUC__ == 4 && __GNUC_MINOR__ >= 4) || __GNUC__ > 4
#	define _m_pslldi(m1, m2) __builtin_ia32_pslldi ((__m64)m1, m2)
#	define _m_psradi(m1, m2) __builtin_ia32_psradi ((__m64)m1, m2)
#	define _m_psrldi(m1, m2) __builtin_ia32_psrldi ((__m64)m1, m2)
#else
#	define _m_pslldi(m1, m2) __builtin_ia32_pslld (m1, m2)
#	define _m_psradi(m1, m2) __builtin_ia32_psrad (m1, m2)
#	define _m_psrldi(m1, m2) __builtin_ia32_psrld (m1, m2)
#endif

#define _m_psubd(m1, m2) __builtin_ia32_psubd (m1, m2)
#define _m_punpckhdq(m1, m2) __builtin_ia32_punpckhdq (m1, m2)
#define _m_punpckldq(m1, m2) __builtin_ia32_punpckldq (m1, m2)
#define _mm_empty() __builtin_ia32_emms ()
#define _mm_set_pi32(m1, m2) { m2, m1 }
#define _mm_set1_pi32(m) { m, m }

#else
#include <mmintrin.h>
#endif

#endif //OPT_MMX

#endif

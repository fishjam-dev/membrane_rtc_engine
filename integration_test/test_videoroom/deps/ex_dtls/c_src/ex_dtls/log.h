#ifdef CNODE_DEBUG
#define DEBUG(X, ...) fprintf(stderr, X "\r\n", ##__VA_ARGS__)
#else
#define DEBUG(...)
#endif

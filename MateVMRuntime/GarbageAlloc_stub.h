#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsInt64 mallocObjectGC_stackstrace(HsInt64 a1, HsInt64 a2, HsInt a3);
extern HsInt64 mallocObjectGC(HsInt a1);
extern void printMemoryUsage(void);
extern void printGCStats(void);
#ifdef __cplusplus
}
#endif


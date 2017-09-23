#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsInt32 mallocObjectGC_stackstrace(HsInt32 a1, HsInt32 a2, HsInt a3);
extern HsInt32 mallocObjectGC(HsInt a1);
extern void printMemoryUsage(void);
extern void printGCStats(void);
#ifdef __cplusplus
}
#endif


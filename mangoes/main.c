#include <stdio.h>

void sift_down(long arr[], int start, int end);
void heap_takek(long arr[], int k, int len);
void print_heap(long arr[], int len);
void heap_sort(long *arr, int len);

int main()
{
    int n;
    long m;
    scanf("%d %ld", &n, &m);
    long a[n];
    long h[n];
    int i;
    for (i = 0; i < n; i++)
        scanf("%ld", &a[i]);
    for (i = 0; i < n; i++)
        scanf("%ld", &h[i]);

    for(i = 0; i < n; i++)
        a[i] += (n-1) * h[i];
    long s = 0;
    for(i = 0; i < n; i++)
        s += a[i];
    printf("%ld\n", s);
    for(i = n; i > 0; i--)
    {
        //print_heap(a, n);
        //heap_takek(a, i, n);
        heap_sort(a, n);
        //print_heap(a, n);
        long sum = 0;
        int j;
        for(j = 0; j < i; j++)
            sum += a[j];
        if (sum <= m)
        {
            printf("%ld,%ld,%d\n", m,sum,i);
            return 0;
        }
        for(j = 0; j < n; j++)
            a[j] -= h[j];
    }
    return 0;
}

void print_heap(long *arr, int len)
{
    int i;
    for(i = 0; i < len; i++)
        printf("%ld ", arr[i]);
    printf("\n");
}

void sift_down(long *arr, int start, int end) {
  int parent = start;
  int child = parent * 2 + 1;
  while (child <= end) {
    if (child + 1 <= end && arr[child] < arr[child + 1]) child++;
    if (arr[parent] >= arr[child])
      return;
    else {
      arr[parent] ^= arr[child];
      arr[child] ^= arr[parent];
      arr[parent] ^= arr[child];
      parent = child;
      child = parent * 2 + 1;
    }
  }
}

void heap_takek(long *arr, int k, int len) {
    int i;
    for(i = (k - 1 - 1) / 2; i >= 0; i--) sift_down(arr, i, k - 1);
    for(i = k; i < len; i++)
    {
        if (arr[0] > arr[i])
        {
            arr[0] ^= arr[i];
            arr[i] ^= arr[0];
            arr[0] ^= arr[i];
            sift_down(arr, 0, k - 1);
        }
    }
}

void heap_sort(long *arr, int len) {
    int i;
  for (i = (len - 1 - 1) / 2; i >= 0; i--) sift_down(arr, i, len - 1);
  for (i = len - 1; i > 0; i--) {
      arr[0] ^= arr[i];
      arr[i] ^= arr[0];
      arr[0] ^= arr[i];
    sift_down(arr, 0, i - 1);
  }
}


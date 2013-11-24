PrinciplesOfReactiveProgramming
===============================


nginx load test result
```
  8 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.94ms   22.06ms 249.62ms   98.22%
    Req/Sec     5.60k     2.06k   13.11k    68.02%
  2545402 requests in 1.00m, 655.30MB read
Requests/sec:  42424.82
Transfer/sec:     10.92MB
```
node scala load test result

```
  8 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    72.54ms  301.75ms   3.12s    98.08%
    Req/Sec   335.56    163.54     1.49k    74.00%
  160277 requests in 1.00m, 20.34MB read
  Socket errors: connect 0, read 39055, write 0, timeout 26
  Non-2xx or 3xx responses: 39055
Requests/sec:   2671.19
Transfer/sec:    347.20KB
```

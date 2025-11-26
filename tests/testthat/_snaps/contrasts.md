# ncs_contrasts() works

    Code
      print(change_from_baseline_tibble_actual, width = Inf, n = Inf)
    Output
      # A tibble: 63 x 10
         arm     time_obs_cont subgroup  estimate    SE    df lower.CL upper.CL
         <fct>           <dbl> <fct>        <dbl> <dbl> <dbl>    <dbl>    <dbl>
       1 control          0.25 subgroup1   -1.28  0.250  36.3   -1.79   -0.772 
       2 active1          0.25 subgroup1   -0.761 0.256  35.3   -1.28   -0.241 
       3 active2          0.25 subgroup1   -1.52  0.279  35.7   -2.08   -0.949 
       4 control          0.5  subgroup1   -2.37  0.412  37.0   -3.21   -1.54  
       5 active1          0.5  subgroup1   -1.38  0.417  36.6   -2.22   -0.531 
       6 active2          0.5  subgroup1   -2.68  0.456  36.3   -3.60   -1.76  
       7 control          0.75 subgroup1   -3.10  0.429  37.1   -3.97   -2.23  
       8 active1          0.75 subgroup1   -1.70  0.434  37.9   -2.58   -0.826 
       9 active2          0.75 subgroup1   -3.15  0.464  36.4   -4.09   -2.21  
      10 control          1    subgroup1   -3.46  0.412  34.1   -4.29   -2.62  
      11 active1          1    subgroup1   -1.74  0.459  34.2   -2.68   -0.811 
      12 active2          1    subgroup1   -2.98  0.443  33.7   -3.88   -2.08  
      13 control          1.25 subgroup1   -3.64  0.429  29.9   -4.51   -2.76  
      14 active1          1.25 subgroup1   -1.65  0.491  30.5   -2.65   -0.646 
      15 active2          1.25 subgroup1   -2.61  0.471  29.8   -3.57   -1.65  
      16 control          1.5  subgroup1   -3.81  0.371  32.3   -4.56   -3.05  
      17 active1          1.5  subgroup1   -1.55  0.393  36.0   -2.35   -0.751 
      18 active2          1.5  subgroup1   -2.42  0.397  32.7   -3.23   -1.61  
      19 control          1.75 subgroup1   -3.99  0.441  34.9   -4.89   -3.10  
      20 active1          1.75 subgroup1   -1.46  0.605  34.0   -2.69   -0.231 
      21 active2          1.75 subgroup1   -2.39  0.477  35.1   -3.36   -1.43  
      22 control          0.25 subgroup2   -1.59  0.273  38.0   -2.15   -1.04  
      23 active1          0.25 subgroup2   -0.798 0.259  34.2   -1.32   -0.271 
      24 active2          0.25 subgroup2   -0.806 0.253  35.5   -1.32   -0.293 
      25 control          0.5  subgroup2   -2.77  0.445  39.7   -3.67   -1.87  
      26 active1          0.5  subgroup2   -1.34  0.425  34.0   -2.21   -0.480 
      27 active2          0.5  subgroup2   -1.42  0.415  36.8   -2.26   -0.582 
      28 control          0.75 subgroup2   -3.12  0.461  41.1   -4.05   -2.19  
      29 active1          0.75 subgroup2   -1.39  0.432  33.1   -2.27   -0.514 
      30 active2          0.75 subgroup2   -1.67  0.436  38.3   -2.55   -0.786 
      31 control          1    subgroup2   -2.88  0.463  35.8   -3.81   -1.94  
      32 active1          1    subgroup2   -1.04  0.400  31.7   -1.86   -0.225 
      33 active2          1    subgroup2   -1.61  0.455  35.5   -2.53   -0.686 
      34 control          1.25 subgroup2   -2.92  0.481  30.8   -3.90   -1.94  
      35 active1          1.25 subgroup2   -0.742 0.420  29.6   -1.60    0.117 
      36 active2          1.25 subgroup2   -1.58  0.487  31.6   -2.57   -0.587 
      37 control          1.5  subgroup2   -3.93  0.390  36.5   -4.72   -3.14  
      38 active1          1.5  subgroup2   -0.851 0.373  30.7   -1.61   -0.0899
      39 active2          1.5  subgroup2   -1.84  0.397  35.7   -2.65   -1.04  
      40 control          1.75 subgroup2   -5.63  0.599  35.0   -6.85   -4.42  
      41 active1          1.75 subgroup2   -1.26  0.450  33.6   -2.18   -0.348 
      42 active2          1.75 subgroup2   -2.33  0.531  35.0   -3.41   -1.25  
      43 control          0.25 subgroup3   -0.743 0.277  35.4   -1.31   -0.181 
      44 active1          0.25 subgroup3    0.251 0.259  34.6   -0.275   0.777 
      45 active2          0.25 subgroup3   -0.949 0.257  40.7   -1.47   -0.430 
      46 control          0.5  subgroup3   -1.41  0.450  36.4   -2.33   -0.503 
      47 active1          0.5  subgroup3    0.267 0.422  35.5   -0.589   1.12  
      48 active2          0.5  subgroup3   -1.72  0.421  41.4   -2.57   -0.868 
      49 control          0.75 subgroup3   -1.95  0.453  37.7   -2.86   -1.03  
      50 active1          0.75 subgroup3   -0.173 0.432  36.4   -1.05    0.703 
      51 active2          0.75 subgroup3   -2.13  0.427  41.2   -2.99   -1.27  
      52 control          1    subgroup3   -2.37  0.436  35.5   -3.25   -1.48  
      53 active1          1    subgroup3   -0.942 0.433  34.6   -1.82   -0.0622
      54 active2          1    subgroup3   -2.23  0.395  35.7   -3.04   -1.43  
      55 control          1.25 subgroup3   -2.82  0.463  31.0   -3.76   -1.87  
      56 active1          1.25 subgroup3   -1.54  0.477  30.8   -2.51   -0.564 
      57 active2          1.25 subgroup3   -2.32  0.411  29.9   -3.16   -1.48  
      58 control          1.5  subgroup3   -3.41  0.392  34.9   -4.20   -2.61  
      59 active1          1.5  subgroup3   -1.57  0.406  33.3   -2.40   -0.746 
      60 active2          1.5  subgroup3   -2.61  0.358  32.6   -3.33   -1.88  
      61 control          1.75 subgroup3   -4.10  0.560  35.5   -5.24   -2.96  
      62 active1          1.75 subgroup3   -1.20  0.479  35.8   -2.18   -0.232 
      63 active2          1.75 subgroup3   -3.05  0.458  36.3   -3.98   -2.12  
         t.ratio  p.value
           <dbl>    <dbl>
       1  -5.12  1.03e- 5
       2  -2.97  5.33e- 3
       3  -5.43  4.13e- 6
       4  -5.75  1.34e- 6
       5  -3.30  2.16e- 3
       6  -5.88  9.88e- 7
       7  -7.22  1.41e- 8
       8  -3.93  3.51e- 4
       9  -6.79  5.86e- 8
      10  -8.39  8.37e-10
      11  -3.80  5.73e- 4
      12  -6.72  1.08e- 7
      13  -8.47  1.92e- 9
      14  -3.36  2.13e- 3
      15  -5.54  5.16e- 6
      16 -10.3   1.09e-11
      17  -3.94  3.62e- 4
      18  -6.09  7.53e- 7
      19  -9.05  1.13e-10
      20  -2.41  2.13e- 2
      21  -5.02  1.50e- 5
      22  -5.85  9.13e- 7
      23  -3.08  4.10e- 3
      24  -3.18  3.02e- 3
      25  -6.22  2.43e- 7
      26  -3.16  3.29e- 3
      27  -3.43  1.50e- 3
      28  -6.78  3.38e- 8
      29  -3.23  2.83e- 3
      30  -3.83  4.65e- 4
      31  -6.22  3.65e- 7
      32  -2.60  1.40e- 2
      33  -3.54  1.15e- 3
      34  -6.07  1.04e- 6
      35  -1.76  8.81e- 2
      36  -3.24  2.79e- 3
      37 -10.1   4.38e-12
      38  -2.28  2.96e- 2
      39  -4.64  4.53e- 5
      40  -9.41  4.04e-11
      41  -2.81  8.27e- 3
      42  -4.38  1.03e- 4
      43  -2.68  1.11e- 2
      44   0.968 3.40e- 1
      45  -3.69  6.52e- 4
      46  -3.14  3.31e- 3
      47   0.633 5.31e- 1
      48  -4.08  1.99e- 4
      49  -4.30  1.17e- 4
      50  -0.400 6.92e- 1
      51  -4.98  1.17e- 5
      52  -5.43  4.14e- 6
      53  -2.17  3.66e- 2
      54  -5.65  2.08e- 6
      55  -6.09  9.63e- 7
      56  -3.22  3.00e- 3
      57  -5.65  3.80e- 6
      58  -8.70  2.88e-10
      59  -3.87  4.80e- 4
      60  -7.28  2.52e- 8
      61  -7.33  1.34e- 8
      62  -2.51  1.66e- 2
      63  -6.67  8.60e- 8

---

    Code
      print(ncs_contrasts(emmeans = emmeans_results_subgroup, type = "change_from_bl",
        time_observed_continuous = "time_obs_cont", time_scheduled_baseline = 0, arm = "arm",
        subgroup = "subgroup", as_tibble = TRUE, confint_args = NULL), width = Inf,
      n = Inf)
    Output
      # A tibble: 63 x 8
         arm     time_obs_cont subgroup  estimate    SE    df t.ratio  p.value
         <fct>           <dbl> <fct>        <dbl> <dbl> <dbl>   <dbl>    <dbl>
       1 control          0.25 subgroup1   -1.28  0.250  36.3  -5.12  1.03e- 5
       2 active1          0.25 subgroup1   -0.761 0.256  35.3  -2.97  5.33e- 3
       3 active2          0.25 subgroup1   -1.52  0.279  35.7  -5.43  4.13e- 6
       4 control          0.5  subgroup1   -2.37  0.412  37.0  -5.75  1.34e- 6
       5 active1          0.5  subgroup1   -1.38  0.417  36.6  -3.30  2.16e- 3
       6 active2          0.5  subgroup1   -2.68  0.456  36.3  -5.88  9.88e- 7
       7 control          0.75 subgroup1   -3.10  0.429  37.1  -7.22  1.41e- 8
       8 active1          0.75 subgroup1   -1.70  0.434  37.9  -3.93  3.51e- 4
       9 active2          0.75 subgroup1   -3.15  0.464  36.4  -6.79  5.86e- 8
      10 control          1    subgroup1   -3.46  0.412  34.1  -8.39  8.37e-10
      11 active1          1    subgroup1   -1.74  0.459  34.2  -3.80  5.73e- 4
      12 active2          1    subgroup1   -2.98  0.443  33.7  -6.72  1.08e- 7
      13 control          1.25 subgroup1   -3.64  0.429  29.9  -8.47  1.92e- 9
      14 active1          1.25 subgroup1   -1.65  0.491  30.5  -3.36  2.13e- 3
      15 active2          1.25 subgroup1   -2.61  0.471  29.8  -5.54  5.16e- 6
      16 control          1.5  subgroup1   -3.81  0.371  32.3 -10.3   1.09e-11
      17 active1          1.5  subgroup1   -1.55  0.393  36.0  -3.94  3.62e- 4
      18 active2          1.5  subgroup1   -2.42  0.397  32.7  -6.09  7.53e- 7
      19 control          1.75 subgroup1   -3.99  0.441  34.9  -9.05  1.13e-10
      20 active1          1.75 subgroup1   -1.46  0.605  34.0  -2.41  2.13e- 2
      21 active2          1.75 subgroup1   -2.39  0.477  35.1  -5.02  1.50e- 5
      22 control          0.25 subgroup2   -1.59  0.273  38.0  -5.85  9.13e- 7
      23 active1          0.25 subgroup2   -0.798 0.259  34.2  -3.08  4.10e- 3
      24 active2          0.25 subgroup2   -0.806 0.253  35.5  -3.18  3.02e- 3
      25 control          0.5  subgroup2   -2.77  0.445  39.7  -6.22  2.43e- 7
      26 active1          0.5  subgroup2   -1.34  0.425  34.0  -3.16  3.29e- 3
      27 active2          0.5  subgroup2   -1.42  0.415  36.8  -3.43  1.50e- 3
      28 control          0.75 subgroup2   -3.12  0.461  41.1  -6.78  3.38e- 8
      29 active1          0.75 subgroup2   -1.39  0.432  33.1  -3.23  2.83e- 3
      30 active2          0.75 subgroup2   -1.67  0.436  38.3  -3.83  4.65e- 4
      31 control          1    subgroup2   -2.88  0.463  35.8  -6.22  3.65e- 7
      32 active1          1    subgroup2   -1.04  0.400  31.7  -2.60  1.40e- 2
      33 active2          1    subgroup2   -1.61  0.455  35.5  -3.54  1.15e- 3
      34 control          1.25 subgroup2   -2.92  0.481  30.8  -6.07  1.04e- 6
      35 active1          1.25 subgroup2   -0.742 0.420  29.6  -1.76  8.81e- 2
      36 active2          1.25 subgroup2   -1.58  0.487  31.6  -3.24  2.79e- 3
      37 control          1.5  subgroup2   -3.93  0.390  36.5 -10.1   4.38e-12
      38 active1          1.5  subgroup2   -0.851 0.373  30.7  -2.28  2.96e- 2
      39 active2          1.5  subgroup2   -1.84  0.397  35.7  -4.64  4.53e- 5
      40 control          1.75 subgroup2   -5.63  0.599  35.0  -9.41  4.04e-11
      41 active1          1.75 subgroup2   -1.26  0.450  33.6  -2.81  8.27e- 3
      42 active2          1.75 subgroup2   -2.33  0.531  35.0  -4.38  1.03e- 4
      43 control          0.25 subgroup3   -0.743 0.277  35.4  -2.68  1.11e- 2
      44 active1          0.25 subgroup3    0.251 0.259  34.6   0.968 3.40e- 1
      45 active2          0.25 subgroup3   -0.949 0.257  40.7  -3.69  6.52e- 4
      46 control          0.5  subgroup3   -1.41  0.450  36.4  -3.14  3.31e- 3
      47 active1          0.5  subgroup3    0.267 0.422  35.5   0.633 5.31e- 1
      48 active2          0.5  subgroup3   -1.72  0.421  41.4  -4.08  1.99e- 4
      49 control          0.75 subgroup3   -1.95  0.453  37.7  -4.30  1.17e- 4
      50 active1          0.75 subgroup3   -0.173 0.432  36.4  -0.400 6.92e- 1
      51 active2          0.75 subgroup3   -2.13  0.427  41.2  -4.98  1.17e- 5
      52 control          1    subgroup3   -2.37  0.436  35.5  -5.43  4.14e- 6
      53 active1          1    subgroup3   -0.942 0.433  34.6  -2.17  3.66e- 2
      54 active2          1    subgroup3   -2.23  0.395  35.7  -5.65  2.08e- 6
      55 control          1.25 subgroup3   -2.82  0.463  31.0  -6.09  9.63e- 7
      56 active1          1.25 subgroup3   -1.54  0.477  30.8  -3.22  3.00e- 3
      57 active2          1.25 subgroup3   -2.32  0.411  29.9  -5.65  3.80e- 6
      58 control          1.5  subgroup3   -3.41  0.392  34.9  -8.70  2.88e-10
      59 active1          1.5  subgroup3   -1.57  0.406  33.3  -3.87  4.80e- 4
      60 active2          1.5  subgroup3   -2.61  0.358  32.6  -7.28  2.52e- 8
      61 control          1.75 subgroup3   -4.10  0.560  35.5  -7.33  1.34e- 8
      62 active1          1.75 subgroup3   -1.20  0.479  35.8  -2.51  1.66e- 2
      63 active2          1.75 subgroup3   -3.05  0.458  36.3  -6.67  8.60e- 8


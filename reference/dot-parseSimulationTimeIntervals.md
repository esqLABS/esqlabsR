# Parse simulation time intervals from string format

Parse simulation time intervals from string format

## Usage

``` r
.parseSimulationTimeIntervals(simulationTimeIntervalsString)
```

## Arguments

- simulationTimeIntervalsString:

  Character string. A string containing simulation time intervals in the
  format "start1,end1,resolution1;start2,end2,resolution2;...". Each
  interval consists of start time, end time, and resolution separated by
  commas, and multiple intervals are separated by semicolons.

## Value

A list of numeric vectors, each containing three elements representing
start_time, end_time, resolution for each time interval. Returns `NULL`
if the input string is `NULL`.

## Details

Parses a string representation of simulation time intervals into a list
of numeric vectors. Each vector contains three elements: start_time,
end_time, resolution. The function validates that all values are
numeric, positive, and that start times are less than end times.

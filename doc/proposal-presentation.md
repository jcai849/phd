---
title: A Platform for Large-scale Statistical Modelling using R
author: Jason Cairns
date: 2020-05-18
---

# Introduction

- _Real-time_ _flexible_ platform for _modelling_ with _larger-than-memory_ datasets
```
Package:            largeScaleR
Type:               Package
Title:              Provides a Distributed Framework for Statistical
                    Modelling
Version:            0.4
```

# Motivation
- Larger-than-Memory datasets
- _e.g._ taxicab: Monthly updated dataset of Taxi trips from TLC
- Naive result: crash/thrash. Why?

# Specifications
- A platform enabling creation of novel models for larger-than-memory datasets
- Interactive
- Simple to use and setup; minimal difference to using existing system
- Fast
- Robust
- Extensible

# Local Approaches
- disk.frame: File-backed dataframes
- parallelism, multicore

--------

![](doc/diskframe.svg)

# Approaches Outside of R
- Hadoop (HDFS, _MapReduce_)
- Spark (RDS): Scala; response to Hadoop; RDD, Dataset API
- MPI: C, C++, Fortran; de-facto standard for HPC; explicit point-to-point communications
- Dask: Python; Task scheduling, distributed data structures

--------

![](doc/mapreduce.svg)

# Distributed Approaches Within R
- SNOW: Split list and map over multiple processes
- pbdR, pbdDMAT: R frontend to MPI
- SparklyR: R frontend to Spark

--------

![](doc/snow.svg)

# Preliminary Results
- Initial package development
- Cluster initialisation
- Object distribution
- Demonstration

--------

![](doc/distobjref.svg)

# Preliminary Results in Detail
- Queue communication
- Worker evaluation
- `distribute()`; `split()`
- `do.dcall(what, args)`
- Generics
- `emerge()`; `combine()`
- Demonstration

--------

![](doc/distobjcomm.svg)



# Issues
- Communication: Queues, responses
- Evaluation & Alignment: Recycling over distributed arrays
- Asynchrony: Race conditions, dependencies
- Debugging: Distributed errors

# Further Work
- Fault Tolerance: $P(\textrm{failure})=1-(1-p)^n$ as $n \to \infty$
- Memory usage optimisation
- Interfacing with other systems
- Benchmarking

# Demonstration & Questions
- Taxicab dataset
- Objective: Determine tips by passenger number for CMT taxis
- 32 processes over 8 nodes

# Contact
- [jcai849/phd](github.com/jcai849/phd)
- [jcai849/largeScaleR](github.com/jcai849/largeScaleR)

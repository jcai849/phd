---
title: A Platform for Large-scale Statistical Modelling using R
author: Jason Cairns
date: 2021-05-18
---

# Introduction

- **Real-time** **flexible** and **extensible** platform for **modelling** with **larger-than-memory** datasets in **R**
```
Package:            largeScaleR
Type:               Package
Title:              Provides a Distributed Framework 
                    for Statistical Modelling
Version:            0.4
```

# Motivation
- Larger-than-Memory datasets
- _e.g._ taxicab: Monthly updated dataset of Taxi trips from TLC
- ~2.5Gb/month since 2009 (~1/3Tb)
- Naive result: crash/thrash. Why?

# Specifications
- A platform enabling creation of novel models for larger-than-memory datasets
- **Interactive**
- Simple to use and setup; minimal difference to using existing system
- Fast
- Robust
- **Extensible**

# Local Approaches
## Using R
- disk.frame
- multicore

# disk.frame

File-backed dataframes

![](doc/diskframe.svg)

# Distributed Approaches
## Outside of R
- MPI: C, C++, Fortran; de-facto standard for HPC; explicit point-to-point communications
- Hadoop (HDFS, _MapReduce_)
- Spark (RDS): Scala; response to Hadoop; RDD, Dataset API
- Dask: Python; Task scheduling, distributed data structures

# MapReduce with Hadoop

![](doc/mapreduce.svg)

# Distributed Approaches
## Using R
- SNOW
- pbdR, pbdDMAT: R frontend to MPI
- SparklyR: R frontend to Spark

# SNOW

Split list and map over multiple processes

![](doc/snow.svg)

# Preliminary Results
- Initial package development
- Cluster initialisation
- Object distribution
- Distributed Object interaction

--------

![](doc/distobjref.svg)

# Preliminary Results in Detail
- Queue communication
- Worker evaluation

--------

![](doc/distobjcomm.svg)

# Preliminary Results in Detail
- `distribute()`
- `do.dcall(what, args)`
- `emerge()`
- Generics

# Main Demonstration
- First 3 months 2011 Taxicab dataset (32M rows)
- Determine total tips
- Create plots of pickup & dropoff locations
- 32 processes over 8 nodes

# Challenges 
- Evaluation & Alignment: Recycling over distributed arrays
- Asynchrony: Race conditions, dependencies
- Debugging: Distributed errors

# Further Work
- Fault Tolerance: $P(\textrm{failure})=1-(1-p)^n$ as $n \to \infty$
- Memory usage optimisation
- Interfacing with other systems
- Benchmarking

# Contact

GitHub

- [jcai849/phd](https://github.com/jcai849/phd)
- [jcai849/largeScaleR](https://github.com/jcai849/largeScaleR)

---
title: A Platform for Large-scale Statistical Modelling using R
author: Jason Cairns
date: 2020-05-18
---

# Introduction

- **largeScaleR** R package
- Real-time flexible platform for modelling with larger-than-memory datasets

# Motivation
- taxicab example
- computer memory constraints

# Local Approaches
- disk.frame
- parallelism, multicore

# Approaches Outside of R
- Hadoop
- Spark
- MPI
- Dask

# Distributed Approaches Within R
- SNOW
- pbdR, pbdDMAT
- SparklyR

# Preliminary Results
- Initial package development
- Cluster initialisation
- Distributed Objects, Queues
- Manipulations
- Example

# Issues
- Communication
- Evaluation & Alignment
- Asynchrony
- Debugging

# Further Work
- Fault Tolerance
- Memory usage optimisation
- Interfacing with other systems
- Benchmarking

# Demonstration
- Taxicab dataset
- Objective: Determine tips by passengers
- 32 processes over 8 nodes

# Contact
- github repositories

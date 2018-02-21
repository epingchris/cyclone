#! /bin/sh
#$ -S /bin/sh
#$ -N cyclone
#$ -cwd
#$ -M epingchris@gmail.com
#$ -m eas
#$ -o cyclone_output.o
#$ -e cyclone_error.e
#$ -pe mpi 16
#$ -l memq
#$ -l mem_free=500M,h_vmem=96G
R CMD BATCH cyclone_paral.R
set -e
rm results_bench/*
_build/default/benchmark.exe NNPlus CDN Clocks LAreg LAstr LBstr
mkdir results_1
mv results_bench/* results_1/
_build/default/benchmark.exe NNPlus CDN Clocks LAreg LAstr LBstr
mkdir results_2
mv results_bench/* results_2/
_build/default/benchmark.exe NNPlus CDN Clocks LAreg LAstr LBstr
mkdir results_3
mv results_bench/* results_3/
_build/default/benchmark.exe NNPlus CDN Clocks LAreg LAstr LBstr
mkdir results_4
mv results_bench/* results_4/
_build/default/benchmark.exe NNPlus CDN Clocks LAreg LAstr LBstr
mkdir results_5
mv results_bench/* results_5/




#!/bin/bash
cd /srv/shiny-server
git clone https://gist.github.com/f9dd35b71dc3e73b845160018ecacea2.git SP_Render
git clone https://gist.github.com/94725c02cbc44488fad4e09e19bcacb7e6.git Hothand
git clone https://gist.github.com/47c01f1224017dc3ec53.git Hierarchical_Models
git clone https://gist.github.com/b0d5b2bc2af9bd6034d9.git Heaping
git clone https://gist.github.com/bd0400c7ce3aacfa4973.git 3d_regression
git clone https://gist.github.com/8e898319968d31b27310.git Corr_Reg_Game
git clone https://gist.github.com/d7ed9873137267ee557b.git Sampling_Distribution
git clone https://gist.github.com/cc55e764b757d8729963.git RandVarGen
git clone https://gist.github.com/200f26d243f4f5bb7334.git Prob_View
git clone https://gist.github.com/fad8ef712fc6f726640c.git ANOVA_robust
git clone https://gist.github.com/48dc47f3ff436aba4b19.git t_Test
git clone https://gist.github.com/8f525780393a05e7d0fd.git WilcoxonMW
git clone https://gist.github.com/1d63ae1c5c5e3a4a5969.git ChaosGame3D
git clone https://gist.github.com/d40a02fa87508ac5ac4b.git ChaosGame2D
git clone https://gist.github.com/94fe941ab0d8a4f36d8b.git BenfordData
git clone https://gist.github.com/96f9adc5c37f4414fbd1.git Gamblers_Ruin
git clone https://gist.github.com/f4475cbfe4cc77cef168.git BenfordSeq
git clone https://gist.github.com/8ace862e9e43f8e29d43.git MLE_Binomial
git clone https://gist.github.com/d896c5848934484181be.git LCO_CI_Generator
git clone https://gist.github.com/eee9a9e00dd4ddd68614.git Longest_Run
/usr/bin/shiny-server.sh
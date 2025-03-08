# 🍃BreezeOmics
## 1. Introduction
**BreezeOmics**: A whole-stream RNA-seq analysis tools, fully graphical design requires no programming knowledge. With the built-in BreezeOmics-Index, BreezeOmics-RNAseq pipeline allows users to perform downstream RNAseq analysis using the HISAT2-StringTie pipeline, and the BreezeOmics-Vis developed based on R shiny enables users to perform advanced downstream visual analysis based on gene expression matrix on the windows system.
## 2. Operating requirement
1. Windows operating system (windows10 and windows11 have been tested).
2. Due to UTF-8 encoding issues, only Windows 10 version 1903 （released on May 21, 2019） and above are supported.
3. At least 50GB hard disk and 8GB memory free.
## 3. Installation
Clone the BreezeOmics project(670Mb) locally and unzip it, and can be used immediately after running **BreezeOmics.exe**, and currently only supports windows system (it has been tested to be stable on windows 10 and windows 11)
## 4. Demo data sets
1. We provide a deleted-quantified [RNA-seq sequencing file (FASTQ)](https://github.com/Liujh0223/testdata) for testing upstream analysis of the BreezeOmics-Index and BreezeOmics-RNAseq pipeline components.
2. Test data for downstream analysis is built into the ["BreezeOmics-Vis" → "GUIDE" → "Sample data for testing"], and can be downloaded and saved locally for use.
## 5. License
1. The entire software package is governed by an End-User License Agreement (EULA), the key restrictions is for personal and non-commercial use only. For details, please refer to the LICENSE file.
2. The BreezeOmics_shiny folder contains R source code for the BreezeOmics-Vis module, this portion of the project is licensed under the MIT License (Non-Commercial Use Only). For details, please refer to the LICENSE-BreezeOmics_shiny file.
## 6. Citation
[1] Jianhong Liu. (2025). BreezeOmics: a visual interactive toolkit for end-to-end transcriptomic analysis (Version 1.0.0) \[Computer software\]. https://github.com/Liujh0223/BreezeOmics
## 7. History versions 
🔗BreezeOmics_beta20240819  [**(BaiduNetdisk)**](https://pan.baidu.com/s/1klkWNWbdEv-VzU3ILnsrMQ?pwd=6666)  
🔗BreezeOmics_V1.0.0  [**(Google Drive)**](https://drive.google.com/file/d/1fvGGp5MlWqJ3HZsMKQmueFcHDQkqubVj/view?usp=sharing)  [**(OneDrive)**](https://1drv.ms/u/c/5cc33f83b4314f09/EVPwzYjOtulPvwB_gPclXTsB2AzkA0eeDfEHEYT9TapSvg?e=OB2Gwj)  [**(BaiduNetdisk)**](https://pan.baidu.com/s/1i6EyiBy2ZnmlqHRc2XKmFg?pwd=ujze)


## 8. Update history
BreezeOmics_beta20240819 --> BreezeOmics_V1.0.0
1. Add the ability to delete established genomes in BreezeOmics-Index.
2. Add genome annotation file format validation in BreezeOmics-Index to ensure that the third column includes exon and mRNA/transcript information, and the ninth column contains parent information (Parent=).
3. Optimize logic and fix bugs in BreezeOmics-Vis. For detailed updates, refer to "BreezeOmics-Vis" → "GUIDE" → "Update history".

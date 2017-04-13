# Project 4: Who Is Who -- Entity Resolution

### [Project Description](doc/project4_desc.md)

Term: Spring 2017

+ Team # 10
+ Project title: Name disambiguation in author citations using Spectral clustering(paper 3) & EM algorithm with C6 tau-coauthor constraints(paper 6)

+ Team members
	+ team member 1 Zhengyuan Guo
	+ team member 2 Yitong Hu
	+ team member 3 Ka Heng Lo
	+ team member 4 Yi Xiang
	+ team member 5 Jinru Xue
	
+ Project summary: 

With the assigned paper 3 & 6 introducing two methods of dealing with name disambiguation in author citations,we study and implement the algorithms. For paper 3, we used K-way spectral clustering method. For paper 6, we used only c6 out of the six types of constraints, then implement EM algorithm to minimize objective function related to the distance between papers and between papers and authors.

For evaluation part,we compare the precision,recall,F1 and accuracy of the two models side by side, and we consider whether the model is robust or not,like whether their performances have much to do with the amount of data given, also the time spent and difficulties to implement the algorithm.


+ Project schedule:

4.1-4.2 (SAT-SUN)
-read papers
-consider every detail in the implement of the algorithm
-record the problems

4.3-4.6(MON-THURS)
-go to office hour to solve the problems recorded
-algorithm implement

4.7(FRI)
-discussion（evaluation index）
-Q&A on algorithms

4.8-4.9 (SAT-SUN)
-evaluation index design
-evaluation implement

4.10-4.12(MON-WED)
-write report(side-by-side!)

4.13-4.14
-prepare for pre
	
**Contribution statement**: 

Zhengyuan Guo:read paper 3 ; data preprocessing(for paper 3) ; integrate the code for paper 3; write report for evaluation part; prepare for presentation

Yitong Hu:read paper 6 ; implement the EM algorithm ; tune parameters for the model ;test paper 6 code for other data(except Akumar); make plan,assign tasks and arrange discussions; write report for em part

Ka Heng Lo:data cleaning(for paper 6);test paper 6 code for other data(except Akumar)

Yi Xiang:read paper 3 ; learn the code from the package to rewrite the spectral clustering 

Jinru Xue:read paper 6 ; code the construction of constrain matrix(c6); data cleaning(for paper 6);test paper 6 code for other data(except Akumar); write report for C6 constrain part

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.

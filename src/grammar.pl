:- op(1120, xfx, <---).
:- op(1200, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2), dynamic(external/1).
:- discontiguous external/1.
:- discontiguous (<---)/2. 
:- discontiguous (::)/2.
:- discontiguous sort/1. 
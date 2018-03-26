# -*- coding: utf-8 -*-
"""
Created on Fri Mar 16 12:25:57 2018

@author: barrosr
"""

import numpy as np
from matplotlib import pyplot as plt

from scipy.stats import norm
import math
import numpy as np
import pylab
import time
import timeit
from scipy import optimize

def BSPrice(S_0, K, T, vol,r, isCall=True):    
    #"    Compute the Black & Scholes formula    
    #"    @var S_0: spot price at time 0
    #"    @var K: option strike
    #"    @var T: option expiry in years
    #"    @var vol: Black implied volatility
    #"    @var vol: the risk-free interest rate
    #"    @var isCall: True if it is a call option, False if it is a put
    
    option_value = 0
    if T * vol == 0.0:
        if isCall:
            option_value = max(S_0 - K, 0.0)
        else:
            option_value = max(K - S_0, 0.0)
    else:
        d1 = (np.log(np.float(S_0)/np.float(K))+(r+0.5*vol**2)*T)/(vol*np.sqrt(T))
        d2=d1-vol*np.sqrt(T)
        if isCall:
            option_value = S_0 * norm.cdf(d1) - K*np.exp(-r*T)*norm.cdf(d2)
        else:
            option_value = K*np.exp(-r*T)*norm.cdf(-d2)-S_0 * norm.cdf(-d1)
    return option_value

def monte_carlo (n_paths=100, n_steps=10, T=1.0, mu=0.03, sigma=0.1, S0=100, dW = None, sde=None):
    S = np.zeros ((n_paths, n_steps + 1))
    S[:,0] = S0
    dt  = T / n_steps    
    
    if dW is None:
        dW = np.random.randn(n_paths, n_steps)
    
    dS_overS = (mu - 0.5 * sigma**2) * dt + sigma * dW * dt
    #S[:,1:] = S0 * (1 + np.cumsum(dS_overS, axis=1))
    S[:,1:] = S0 * np.exp( np.cumsum(dS_overS, axis=1))
    
    return S
    
T=10
sigma=0.20
n_steps=10


fn_call = lambda M: np.maximum(M[:,-1] - strike, 0.0).mean () * np.exp(-mu * T)
fn_fwd = lambda M: M[:,-1].mean ()

if False:
    #___________________naive monte-carlo--------------------------------#
    std_list = []
    for n_paths in np.power(10, np.linspace(1, 4, 27)):
        M1 = np.array([fn(monte_carlo(T = T, sigma=sigma, n_paths=int(n_paths))) for i in range(100)])
        std_list.append (M1.std())
    
    fig = plt.figure()
    plt.title("Naive Monte Carlo Simulation")    
    axes = plt.gca ()
    axes.set_xscale ('log')
    axes.set_ylabel ('Standard deviation of S(T)')
    axes.set_xlabel ('Number of paths')
    plt.plot(np.power(10, np.linspace(1, 4, 27)), std_list)
    plt.show ()

    fwd_list = []
    n_paths = 1000    
    for i in range (100):
        M1 = monte_carlo(T = T, sigma=sigma, n_paths=int(n_paths))
        fwd_list.append (fn_fwd(M1))
    fig = plt.figure ()
    plt.title ('Outcome of 100 simulations using 1000 paths')
    plt.hist (fwd_list, 50)
    plt.show ()
        
    #--------------------------------------------------------------------#

if False:
    #---------------anti-sampling-------------------------------------------------#
    anti_std_list = []
    moment_std_list = []
    T = 5.0
    for n_paths in np.power(10, np.linspace(1, 4, 27)):
        arr = np.zeros (100)
        moment_arr = np.zeros (100)
        for i in range (100):
            dW = np.random.randn(int(n_paths), n_steps)
            dW_prime = (dW - dW.mean(axis=0)) / dW.std(axis=0)
            M_prime = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma, dW = dW_prime)    
            dW[-int(n_paths/2):, :] = -dW[0:int(n_paths)/2, :]
            arr[i]= fn(monte_carlo(T = T, sigma = sigma, dW=dW, n_paths=int(n_paths)))
            moment_arr [i] = fn (M_prime)
        anti_std_list.append (arr.std())
        moment_std_list.append (moment_arr.std())
    
    
    fig = plt.figure()
    plt.title("Monte Carlo Simulation - Variance Reduction - T=5y")    
    axes = plt.gca ()
    axes.set_xscale ('log')
    axes.set_ylabel ('Standard deviation of S(T)')
    axes.set_xlabel ('Number of paths')
    plt.plot(np.power(10, np.linspace(1, 4, 27)), std_list, label='Regulater MC')
    plt.plot(np.power(10, np.linspace(1, 4, 27)), anti_std_list, label='Antithetic Sampling')
    plt.plot(np.power(10, np.linspace(1, 4, 27)), moment_std_list, label='Moment Matching')
    plt.legend (loc='best')
    plt.show ()
    
    
 
if False:
    #-------------------------moment matching-----------------------------#
    T = 10
    n_paths = 10000
    fn_fwd = lambda M: np.maximum(M[:,-1] - 0.0, -10000000.0).mean ()
    cf_fwd = S0 * np.exp(mu * T)
    
    strike = cf_fwd
    cf_call = BSPrice(S0, strike, T=T, vol=sigma, r=mu)
    
    raw_call_list = []
    adj_call_list = []
    raw_fwd_list = []
    adj_fwd_list = []
    for i in range (100):
        dW = np.random.randn(int(n_paths), n_steps) #original draws
        M = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma, dW = dW)    
        raw_call_list.append (fn_call(M))
        raw_fwd_list.append (fn_fwd(M))
        
        dW_prime = (dW - dW.mean(axis=0)) / dW.std(axis=0)
        M_prime = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma, dW = dW_prime)    
        adj_call_list.append (fn_call (M_prime))   
        adj_fwd_list.append (fn_fwd (M_prime))   
        
    fig = plt.figure ()
    plt.hist(adj_call_list, bins=50, label='Adj')
    plt.legend ()
    plt.show ()
    
    fig = plt.figure ()
    plt.hist(raw_call_list, bins=50, label='Raw')
    plt.legend (loc='best')
    plt.show ()

    print ('Moment matching '+ str (n_paths) + ' paths')    
    
    print ('Call Raw price standard deviation: ' + str(np.array(raw_call_list).std ()))
    print ('Call Adjusted price standard deviation: ' + str(np.array(adj_call_list).std ()))
    
    print ('Forward Raw price standard deviation: ' + str(np.array(raw_fwd_list).std ()))
    print ('Forward Adjusted price standard deviation: ' + str(np.array(adj_fwd_list).std ())) 
 
if False:   
    #-------------------------variate recycling--------------------------#
    print ('Vega without variate recycling')
    n_paths = 10000
    arr = np.zeros (100)
    d_sigma = 0.01
    for i in range (100):
        arr[i]= (fn_call(monte_carlo(T = T, sigma = sigma + d_sigma, n_paths=int(n_paths))) - fn_call(monte_carlo(T = T, sigma = sigma, n_paths=int(n_paths)))) / d_sigma
    fig = plt.figure ()
    plt.hist(arr, bins=50)
    plt.title ('Vega without variate recycling')
    plt.show ()
    #_--------------------------------------------------------------------------

    #-------------------------variate recycling--------------------------#
    print ('Vega with variate recycling')
    n_paths = 10000
    arr = np.zeros (100)
    d_sigma = 0.01
    for i in range (100):
        dW = np.random.randn(int(n_paths), n_steps)
        dW[-int(n_paths/2):, :] = -dW[0:int(n_paths)/2, :]
        arr[i]= (fn(monte_carlo(T = T, sigma = sigma + d_sigma, dW=dW, n_paths=int(n_paths))) - fn(monte_carlo(T = T, sigma = sigma, dW=dW, n_paths=int(n_paths)))) / d_sigma
    fig = plt.figure ()
    plt.hist(arr, bins=50)
    plt.title ('Vega with variate recycling')
    plt.show ()
    #_--------------------------------------------------------------------------

if False:
    #-------------------------control variate-----------------------------#
    n_paths = 1000
    cf_fwd = S0 * np.exp(mu * T)
    
    strike = cf_fwd
    
    
    cf_call = BSPrice(S0, strike, T=T, vol=sigma, r=mu)
    
    raw_call_list = []
    adj_call_list = []
    for i in range (100):
        M = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma)    
        adj_call = fn_call(M)  + 0.5*(cf_fwd - fn_fwd(M))
        raw_call_list.append (fn_call(M))
        adj_call_list.append(adj_call)
    fig = plt.figure ()
    plt.hist(adj_call_list, bins=50, label='Adj')
    plt.legend ()
    plt.show ()
    
    fig = plt.figure ()
    plt.hist(raw_call_list, bins=50, label='Raw')
    plt.legend (loc='best')
    plt.show ()
    
    print ('Control Variate')
    print ('Raw price standard deviation: ' + str(np.array(raw_call_list).std ()))
    print ('Adjusted price standard deviation: ' + str(np.array(adj_call_list).std ()))

if True:
    #----------------------importance sampling-------------------------------#
    
    n_paths = 10000
    strike = cf_fwd * np.exp (2.5 * sigma * T **0.5)
    cf_call = BSPrice(S0, strike, T=T, vol=sigma, r=mu)
    
    no_paths_list = []
    std_is_list = []
    raw_std_list = []
    
    for theta in np.linspace(0,0.50,11):    
        raw_call_list = []
        adj_call_list = []
        
        raw_exercise_paths_list = []
        adj_exercise_paths_list = []
        adj_fwd_list = []
        
        for i in range (100):
            dW = np.random.randn(int(n_paths), n_steps)
            dW = (dW - dW.mean(axis=0)) / dW.std(axis=0) #moment adjusted
            M = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma, dW = dW)    
            raw_call_list.append (fn_call(M))
            raw_exercise_paths_list.append (np.count_nonzero(np.maximum(M[:,-1] - strike, 0.0)))
                
            #theta = 0.50
            dW_prime = dW + theta * T / n_steps    
            Z = np.exp( theta * np.sum(dW, axis=1) + theta ** 2 * T / 2)
            #M_is = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu+theta*sigma, sigma=sigma, dW=dW)   
            M_is = monte_carlo(n_paths= n_paths, n_steps=10, T=T, mu=mu, sigma=sigma, dW=dW_prime)
        
            adj_call = (np.maximum(M_is[:,-1]-strike, 0) / Z).mean () * np.exp(-mu * T)    
            
            adj_call_list.append(adj_call)
            adj_fwd_list.append (((M_is[:,-1]-cf_fwd) / Z).mean ())
            adj_exercise_paths_list.append (np.count_nonzero(np.maximum(M_is[:,-1] - strike, 0.0)))
            
        no_paths_list.append (np.mean(adj_exercise_paths_list))
        std_is_list.append (np.std(adj_call_list))
        
    fig = plt.figure ()
    plt.title ('Convergence vs Number of Exercised Paths')
    plt.plot (no_paths_list, std_is_list)
    plt.show ()
        
    fig = plt.figure ()
    plt.title ('Call price - 1090 paths in the money')
    plt.hist(adj_call_list, bins=50)
    plt.legend ()
    plt.show ()
    
    fig = plt.figure ()
    plt.title ('Call price - 24 paths in the money')
    plt.hist(raw_call_list, bins=50)
    plt.legend (loc='best')
    plt.show ()
    
    print ('Raw price avg and standard deviation: ' + str(np.array(raw_call_list).mean ()) + ', ' + str(np.array(raw_call_list).std ()))
    print ('Adjusted price avg and standard deviation: ' + str(np.array(adj_call_list).mean ()) + ', ' + str(np.array(adj_call_list).std ()))
    
    print ('Number of paths exercised: ' + str(np.mean (adj_exercise_paths_list)) + ' vs ' + str(np.mean(raw_exercise_paths_list)))



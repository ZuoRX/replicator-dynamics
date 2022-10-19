#coding=utf-8
import numpy as np
from numpy import arange
from numpy import exp
from numpy import sqrt
from numpy import cos
from numpy import e
from numpy import pi
from numpy import meshgrid
from matplotlib import pyplot
from mpl_toolkits.mplot3d import Axes3D
from scipy.optimize import dual_annealing
from numpy.random import rand
from numpy import exp
from numpy import sqrt
from numpy import cos
from numpy import e
from scipy.special import comb, perm



# # objective function
# def objective(x, y):
#     return -20.0 * exp(-0.2 * sqrt(0.5 * (x ** 2 ))) - exp(0.5 * (cos(2 * pi * x) )) + e + 20


# def Two_options(y):
#     M = 200
#     beta = 1.5
#     d = 4
#     N = 100
#     c = 1
#     Pc = 0
#     Pd = 0
#     for n in range(0,N-1):
#         Nd = n
#         pai_d = (beta * d * M) / ((N-1-Nd) * c + (Nd + 1) * beta * d) - d
#         pai_c = (c * M) / ((N-1-Nd + 1) * c + Nd * beta * d) - c
#         Pc += comb(N-1, Nd) * (y ** Nd) * (1-y) ** (N-1-Nd) * pai_c
#         Pd += comb(N-1, Nd) * (y ** Nd) * (1-y) ** (N-1-Nd) * pai_d
#     # （3）群体策略的期望收益
#     R_ = y * Pd + (1 - y) * Pc
#     # (4)复制动态方程
#     y_dot = y * (Pd - R_)
#     return abs(y_dot)
#
# lw = [0.1]
# up = [0.9]

# result = dual_annealing(Two_options, list(zip(lw, up)))
# print(result)

# def Three_options_two_realization(x,y):
#
#     xD = x
#     xC = 1-x
#     # xL = 1-x-y
#
#     M = 200
#     N = 100
#     # 相对效用
#     betaD = 1.5  # 1.不合理的地方：betaD是1.5倍，d确是l的8倍
#     betaC = 1.1
#     # 成本
#     d = 4
#     c = 1
#     l = 0
#     xL = 0
#     # l = 0.5
#     Pd = 0 #内卷群体   的累加净收益
#     Pc = 0 #sit-up群体  的累加净收集
#     Pl = 0 #躺平群体    的累加净收益
#     for Nd in range(0,N-1):
#         Nc = N-1-Nd
#         # for Nc in range(0,N-1-Nd):
#         #     Nl = N-1-Nd-Nc
#         Nl = 0
#
#         pai_d = betaD*d*M/(betaD*(Nd+1)*d +betaC*Nc*c+Nl*l)-d
#         pai_c = betaC*c*M/(betaD*Nd*d+betaC*(Nc+1)*c +Nl*l)-c
#         pai_l =       l*M/(betaD*Nd*d+betaC*Nc*c+(Nl+1)*l)-l
#
#         Pd += comb(N - 1, Nd) * comb(N - 1 - Nd, Nl) * (xD ** Nd) * (xC ** Nc) * (
#                     xL ** Nl) * pai_d
#         Pc += comb(N - 1, Nd) * comb(N - 1 - Nd, Nl) * (xD ** Nd) * (xC ** Nc) * (
#                     xL ** Nl) * pai_c
#
#     R_ = xD * Pd + xC * Pc + xL * Pl  ##均值
#     xD_ = xD * (Pd - R_)
#     xC_ = xC * (Pc - R_)
#     xL_ = xL * (Pl - R_)
#     return xD_

def Three_options(x,y):

    xD = x
    xC = y
    xL = 1-x-y

    M = 200
    N = 50
    # 相对效用
    betaD = 1.5  # 1.不合理的地方：betaD是1.5倍，d确是l的8倍
    betaC = 1.1
    # 成本
    d = 4
    c = 1
    l = 0.5
    Pd = 0 #内卷群体   的累加净收益
    Pc = 0 #sit-up群体  的累加净收集
    Pl = 0 #躺平群体    的累加净收益
    for Nd in range(0,N-1):
        for Nc in range(0,N-1-Nd):
            Nl = N-1-Nd-Nc
            pai_d = betaD*d*M/(betaD*(Nd+1)*d +betaC*Nc*c+Nl*l)-d
            pai_c = betaC*c*M/(betaD*Nd*d+betaC*(Nc+1)*c +Nl*l)-c
            pai_l =       l*M/(betaD*Nd*d+betaC*Nc*c+(Nl+1)*l)-l

            Pd += comb(N - 1, Nd) * comb(N - 1 - Nd, Nl) * (xD ** Nd) * (xC ** Nc) * (
                        xL ** Nl) * pai_d
            Pc += comb(N - 1, Nd) * comb(N - 1 - Nd, Nl) * (xD ** Nd) * (xC ** Nc) * (
                        xL ** Nl) * pai_c
            Pl += comb(N - 1, Nd) * comb(N - 1 - Nd, Nl) * (xD ** Nd) * (xC ** Nc) * (
                        xL ** Nl) * pai_l

    R_ = xD * Pd + xC * Pc + xL * Pl  ##均值
    xD_ = xD * (Pd - R_)
    xC_ = xC * (Pc - R_)
    xL_ = xL * (Pl - R_)
    return xD_+xL_+xC_

r_min, r_max = 0, 1
# sample input range uniformly at 0.1 increments
xaxis = arange(r_min, r_max, 0.01)
yaxis = arange(r_min, r_max, 0.01)
# create a mesh from the axis
x, y = meshgrid(xaxis, yaxis)
# print(x)
# print(y)
# '''[[0.   0.01 0.02 ... 0.47 0.48 0.49]
#  [0.   0.01 0.02 ... 0.47 0.48 0.49]
#  [0.   0.01 0.02 ... 0.47 0.48 0.49]
#  ...
#  [0.   0.01 0.02 ... 0.47 0.48 0.49]
#  [0.   0.01 0.02 ... 0.47 0.48 0.49]
#  [0.   0.01 0.02 ... 0.47 0.48 0.49]]'''

# print(type(x))  ##<class 'numpy.ndarray'>
length = 100
a = []
for i in range(0,length):
    a.append([j/length for j in range(0, length)])
a = np.array(a)
# print(a)
b = []
for i in range(0,length):
    b.append([i/length for j in range(0, length)])
b = np.array(b)
# print(a)
# print(b)
# print(b[0])
# b[1][0] = 2
# print(b[1][0])

for i in range(length):
    for j in range(length):
        if a[i][j] +b[i][j] >1:
            a[i][j] = 0.3
            b[i][j] = 0.3
# print(a)

# a = x
# b = y
# print(a)
# print(b)

results = Three_options(a, b)
# create a surface plot with the jet color scheme
figure = pyplot.figure()
axis = figure.gca(projection='3d')
axis.plot_surface(x, y, results, cmap='jet')
# show the plot
pyplot.show()



        # def ObjectFunction(y):
#     M = 200
#     beta = 1
#     d = 4
#     N = 100
#     c = 1
#     # -----参数解释说明------#
#     # #N个个体
#     # N<-4
#     # #M资源 c(5,15,25)
#     # M<-5
#     # #less effort的成本
#     # c<-1
#     # #投入效用
#     # beta<-1
#     # #more effort的成本
#     # d<-4
#     # -----------------------#
#
#     #
#     # # 策略c（cooperate, less effort）和策略d(defect，more effort)的收益
#     # # （1）个体选择策略c的期望收益
#     # pai_c = (c * M) / ((Nc + 1) * c + Nd * beta * d) - c
#     # # （2）个体选择策略d的期望收益
#     # pai_d = (beta * d * M) / (Nc * c + (Nd + 1) * beta * d) - d
#
#     Pc = 0
#     Pd = 0
#     for n in range(0,N-1):
#         Nd = n
#         pai_d = (beta * d * M) / ((N-1-Nd) * c + (Nd + 1) * beta * d) - d
#         pai_c = (c * M) / ((N-1-Nd + 1) * c + Nd * beta * d) - c
#         Pc += comb(N-1, Nd) * (y ** Nd) * (1-y) ** (N-1-Nd) * pai_c
#         Pd += comb(N-1, Nd) * (y ** Nd) * (1-y) ** (N-1-Nd) * pai_d
#     # （3）群体策略的期望收益
#     R_ = y * Pd + (1 - y) * Pc
#     # (4)复制动态方程
#     y_dot = y * (Pd - R_)
#     return abs(y_dot)
#
# lw = [0.1]
# up = [0.9]
#
#
# result = dual_annealing(ObjectFunction, list(zip(lw, up)))
# print(result)



# def ObjF(x,y):
#
#     M = 200
#     N = 50
#     # 相对效用
#     betaD = 1.5  # 1.不合理的地方：betaD是1.5倍，d确是l的8倍
#     betaC = 1.1
#
#     # 成本
#     d = 4
#     c = 1
#     l = 0.5
#
#     Nd = x * (N - 1) # 1
#     Nc = y * (N - 1) # 3
#     Nl = N - 1 - Nd -Nc  # 2 因为有个减法，所以放在最下面位置，否则保持d,c,l
#
#     # 个体策略选择的期望收益
#     pai_d = betaD * d * M / (betaD * (Nd + 1) * d + betaC * Nc * c + Nl * l) - d
#     pai_c = betaC * c * M / (betaD * Nd * d + betaC * (Nc + 1) * c + Nl * l) - c
#     pai_l = l * M / (betaD * Nd * d + betaC * Nc * c + (Nl + 1) * l) - l
#
#     Pd = 0  # 内卷群体    的累加净收益
#     Pc = 0  # sit-up群体  的累加净收集
#     Pl = 0  # 躺平群体    的累加净收益
#     for Nd
#
#     for (Nd in 0:(N-1))
#     for (Nl in 0:(N - 1 - Nd)){
#         Pd < - Pd + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD ^ Nd) * ((1 - xL - xD) ^ Nc) * (xL ^ Nl) * pai_d
#     Pc < - Pc + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD ^ Nd) * ((1 - xL - xD) ^ Nc) * (xL ^ Nl) * pai_c
#     Pl < - Pl + choose(N - 1, Nd) * choose(N - 1 - Nd, Nl) * (xD ^ Nd) * ((1 - xL - xD) ^ Nc) * (xL ^ Nl) * pai_l
#     }
#     }

# define range for input
# r_min, r_max = 0, 1
# # sample input range uniformly at 0.1 increments
# xaxis = arange(r_min, r_max, 0.01)
# yaxis = arange(r_min, r_max, 0.01)
# # create a mesh from the axis
# x, y = meshgrid(xaxis, yaxis)
# # compute targets
# results = ObjectFunction(x, y)
# # results = objective(x, y)
# # create a surface plot with the jet color scheme
# figure = pyplot.figure()
# axis = figure.gca(projection='3d')
# axis.plot_surface(x, y, results, cmap='jet')
# # show the plot
# pyplot.show()
#














#
# # objective function
# def objective(x, y):
#     return -20.0 * exp(-0.2 * sqrt(0.5 * (x ** 2 + y ** 2))) - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) + e + 20
#
#
# # define range for input
# r_min, r_max = -5.0, 5.0
# # sample input range uniformly at 0.1 increments
# xaxis = arange(r_min, r_max, 0.1)
# yaxis = arange(r_min, r_max, 0.1)
# # create a mesh from the axis
# x, y = meshgrid(xaxis, yaxis)
# # compute targets
# results = objective(x, y)
# # create a surface plot with the jet color scheme
# figure = pyplot.figure()
# axis = figure.gca(projection='3d')
# axis.plot_surface(x, y, results, cmap='jet')
# # show the plot
# pyplot.show()
from collections.abc import Iterable
from Polynomial import Coefficient
from Polynomial import PolynomialExpression
import Helper
import time
import copy
import math

class SymbolicMatrix:
    rows = 0
    columns = 0
    data = []
    FLOATING_POINT_ERROR_THRESHOLD = 0.00000000001

    def __init__(self, *rows, rowCount=None, columnCount=None, data=None):
        if data != None:
            # override standard construction
            self.data = data
        else:
            # typical construction
            self.data = []

            if len(rows) == 0:
                if rowCount == None:
                    rowCount = 1
                if columnCount == None:
                    columnCount = 1
                self.rows = rowCount
                self.columns = columnCount
                for r in range(rowCount):
                    self.data.append([0 for i in range(columnCount)])
            else:
                for row in rows:
                    if not(isinstance(row, Iterable)):
                        row = [row]
                    self.data.append(row)

        self.padArray()
        self.fixFloatingPointErrors()

    def __delitem__(self, key):
        del self.data[key]
        self.rows -= 1

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        if isinstance(value, Iterable):
            self.data[key] = value
            self.padArray()

    def __mul__(self, other):
        return self.multiply(other)

    def __rmul__(self, other):
        return self.multiply(other, reverseOrder=True)

    def __str__(self):
        return self.toString()

    def toString(self):
        longestStringByCol = []
        for r in range(self.rows):
            for c in range(self.columns):
                if c + 1 > len(longestStringByCol):
                    longestStringByCol.append(0)
                longestStringByCol[c] = max(longestStringByCol[c], len(str(self.data[r][c])))
        
        tabsByCol = []
        for c in range(self.columns):
            if c + 1 > len(tabsByCol):
                    tabsByCol.append(0)
            tabsByCol[c] = 1 + (longestStringByCol[c] + 2) // 8

        returnString = ""
        for r in range(self.rows):
            rowString = ""
            if r == 0:
                rowString += "["
            else:
                rowString += " "
            for c in range(self.columns):
                dataString = str(self.data[r][c])
                rowString += dataString
                if c < (self.columns - 1):
                    tabs = tabsByCol[c] - (len(dataString)) // 8
                    for tab in range(tabs):
                        rowString += "\t"
            if r == (self.rows - 1):
                rowString += "]"
            else:
                rowString += "\n"
            returnString += rowString
        return returnString
        
    def padArray(self):
        rowCheck = 0
        colCheck = 0

        for row in self.data:
            rowCheck += 1
            colCheck = max(colCheck, len(row))
        
        self.rows = rowCheck
        self.columns = colCheck

        for i in range(self.rows):
            self.data[i] = self.data[i] + [0]*(self.columns - len(self.data[i]))

    def deleteRows(self, rowsToDelete):
        if not(isinstance(rowsToDelete, Iterable)):
            rowsToDelete = [rowsToDelete]
        
        shift = 0
        for row in sorted(rowsToDelete):
            self.__delitem__(row - shift)
            shift += 1

    def __deleteColumn__(self, column):
        for row in self.data:
            del row[column]
        
        self.columns -= 1

    def deleteColumns(self, columnsToDelete):
        if not(isinstance(columnsToDelete, Iterable)):
            columnsToDelete = [columnsToDelete]
        
        shift = 0
        for col in sorted(columnsToDelete):
            self.__deleteColumn__(col - shift)
            shift += 1

    def submatrix(self, rows, cols):
        newRows = [self.data[i] for i in rows]
        for i in range(len(newRows)):
            newRows[i] = [newRows[i][c] for c in cols]
        
        return SymbolicMatrix(data=newRows)

    def det(self):
        if not(self.rows == self.columns):
            raise Exception("matrix is not square - cannot calculate determinant")
        else:
            # Calculate determinant using minor expansion algorithm
            # M stores the determinants of submatrices
            # Note: the keys for M are frozen sets representing the columns of submatrices (the rows of the submatrix are the first n rows, where n is the size of the set)
            M = {}
         
            # set determinants of size 1 submatrices for the first row (i is the column)
            # the determinant of a size 1 submatrix is itself
            for i in range(0,self.rows):
                M[frozenset([i])] = self.data[0][i]

            # calculate determinant of submatrices of increasing size, using previously-calculated determinants for smaller submatrices
            for i in range(1,self.rows):
                columnCombinations = Helper.combinationsFrozen(range(self.rows), i+1)
                for J in columnCombinations:
                    M[J] = 0
                    cols = sorted(list(J)) # unfreeze and sort combination
                    for k in range(i+1):
                        subcombination = cols.copy()
                        jk = subcombination.pop(k)
                        J_lessjk = frozenset(subcombination)
                        M[J] += (-1) ** (i + k) * self.data[i][jk] * M[J_lessjk]
            
            # return the determinant calculated for the full matrix
            n = frozenset(range(self.rows))
            return M[n]

    def inverse(self):
        augmentedMatrix = SymbolicMatrix(data=self.data)
        for r in range(augmentedMatrix.rows):
            for c in range(augmentedMatrix.rows):
                augmentedMatrix.data[r].append(1 * (r == c))

        augmentedMatrix.columns += augmentedMatrix.rows
        processedMatrix = augmentedMatrix.gaussJordanElimination()
        processedMatrix.deleteColumns(range(augmentedMatrix.rows))
        return processedMatrix

    def swapRows(self, r1, r2):
        temp = self.data[r1]
        self.data[r1] = self.data[r2]
        self.data[r2] = temp

    def divideRow(self, row, divisor):
        self.data[row] = [c / divisor for c in self.data[row]]

    def subtractRowMultiple(self, targetRow, operatorRow, multiple):
        m = copy.deepcopy(multiple)
        for c in range(self.columns):
            if self.data[operatorRow][c] != 0:
                self.data[targetRow][c] -= self.data[operatorRow][c] * m

    def gaussJordanElimination(self):
        M = SymbolicMatrix(data=self.data.copy())
        k = 0
        l = -1
        for k in range(M.rows):
            while l <= M.columns:
                l += 1
                for i in range(k, M.rows):
                    if M.data[i][l] != 0:
                        break
                else:
                    continue
                break

            if M.data[k][l] == 0 and not(k == M.rows):
                for i in range(k+1, M.rows):
                    if M.data[i][l] != 0:
                        M.swapRows(i,k)
                        break

            M.divideRow(k, M.data[k][l])

            for i in range(M.rows):
                if i != k:
                    M.subtractRowMultiple(i,k,M.data[i][l])

        return M
            
    def multiply(self, other, reverseOrder=False):
        newData = []
        if isinstance(other, SymbolicMatrix):
            A = self
            B = other
            if reverseOrder:
                B = self
                A = other

            if A.columns != B.rows:
                raise Exception("incompatible arrays")
            
            for r in range(A.rows):
                newRow = []
                for c in range(B.columns):
                    sum = 0
                    for r2 in range(B.rows):
                        sum += A.data[r][r2] * B.data[r2][c]
                    newRow.append(sum)
                newData.append(newRow)

        elif isinstance(other, (Coefficient, int, float)):
            newData = self.data.copy()
            for r in range(self.rows):
                for c in range(self.columns):
                    newData[r][c] *= other

        else:
            return NotImplemented
            
        return SymbolicMatrix(data=newData)

    def fixFloatingPointErrors(self):
        SIGNIFICANT_FIGURES = 6
        for r in range(self.rows):
            for c in range(self.columns):
                if isinstance(self.data[r][c], float):
                    if abs(self.data[r][c]) < self.FLOATING_POINT_ERROR_THRESHOLD:
                        self.data[r][c] = 0
                    else:
                        roundPosition = SIGNIFICANT_FIGURES - (math.floor(math.log10(abs(self.data[r][c]))) + 1)
                        self.data[r][c] = round(self.data[r][c], roundPosition)

    def solve(self, B):
        return self.inverse() * B

    def solveToList(self, B):
        matrixSolution = self.solve(B)
        return matrixSolution.transpose()[0]

    def transpose(self, inPlace=False):
        newData = []
        for c in range(self.columns):
            newRow = []
            for r in range(self.rows):
                newRow.append(self.data[r][c])
            newData.append(newRow)
        if inPlace:
            self.data = newData
            rows = self.rows
            columns = self.columns
            self.rows = columns
            self.columns = rows
            return self
        else:
            return SymbolicMatrix(data = newData)



# c1 = Coefficient(3,"t")
# c2 = -1 * (Coefficient(1,"t") ** 3) - 4

# M = SymbolicMatrix([-1,2,6,7],[3,-6,0,-3],[1,2,6,-1],[0,4,5,3])

# N = SymbolicMatrix([7],[8],[3],[1])
# print(M)
# print()
# print(N)
# print()
# # print(M.gaussJordanElimination())
# print(M.solve(N))



# def createSylvesterMatrix(A, B):
#     m = B.degree
#     n = A.degree
#     size = m + n

#     sylvesterMatrix = SymbolicMatrix(rowCount = size, columnCount= size)

#     for i in range(m):
#         row = []
#         for j in range(i):
#             row.append(0)
#         coeff_A = A.getCoefficientList()
#         # for c in coeff_A:
#         #     if c.isinstance(Coefficient):
#         #         c = c.evaluate()
#         row.extend(coeff_A)
#         for j in range(size - n - 1 - i):
#             row.append(0)
#         sylvesterMatrix[i] = row
    
#     for i in range(n):
#         row = []
#         for j in range(i):
#             row.append(0)
#         coeff_B = B.getCoefficientList()
#         # for c in coeff_B:
#         #     if c.isinstance(Coefficient):
#         #         c = c.evaluate()
#         row.extend(coeff_B)
#         for j in range(size - m - 1 - i):
#             row.append(0)
#         sylvesterMatrix[m + i] = row

#     return sylvesterMatrix


# A = PolynomialExpression([Coefficient(3,"t"),-1 * (Coefficient(1,"t") ** 3) - 4],[2,0])
# B = PolynomialExpression([1,Coefficient(1,"t") ** 3,-9],[2,1,0])

# M = createSylvesterMatrix(A, B)

# # M = SymbolicMatrix([1,5,4,3],[8,2,6,1],[3,4,1,9],[6,5,3,3])
# print(M)
# print(M.det())








# c1 = Coefficient(3,"t")
# c2 = -1 * (Coefficient(1,"t") ** 3) - 4

# # test = SymbolicMatrix(rowCount = 3, columnCount= 3)
# test = SymbolicMatrix([1,2,c1,1,2,3,4],[1,2,3],[c2,433,4,40,0,0,1],[2,4,40,0,0,1],[12,2,4,40,0,0,1])
# # test.deleteColumns([0,1])
# print(test)
# print("------------------------------")
# print(test.submatrix([0,2,3],[1,2,3,5]))

# a = set([1,2,3,4])
# b = set([2,3,1,4])

# print(a==b)
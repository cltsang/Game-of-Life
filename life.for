cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c main program
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PROGRAM LIFE
c main program variables
      CHARACTER patternName*80, pattern(100, 80)
      INTEGER numGenerations, rowNum, colNum, stillLife
c
      CALL openFile()
      CALL readFile(patternName, numGenerations, rowNum, colNum,pattern)
      CALL closeFile()
      
      CALL run(numGenerations, rowNum, colNum, pattern, stillLife)
      
      CALL writeOutput(patternName, rowNum, colNum, pattern, stillLife,
     &   numGenerations)
	  
      STOP
      END
	  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c detect if the pattern is still life
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE run(numGenerations, rowNum, colNum, pattern, stillLife)
c local variables
      CHARACTER pattern(100, 80), next(100, 80)
      INTEGER numGenerations, rowNum, colNum, num, step, living,row,col
      INTEGER cellState, stillLife
      LOGICAL changed

      stillLife = -1
      step = 1
 0020 IF(step .GT. (numGenerations+1))
     &    GOTO 0029
        CALL copy2DArray(pattern, next, rowNum, colNum)
        changed = .FALSE.
        row = 1
 0021   IF(row .GT. rowNum)
     &      GOTO 0024
          col = 1
 0022     IF(col .GT. colNum)
     &        GOTO 0023
*     WRITE(*,*) 'step', step, ': ', row, ',', col
            num = numLivingNeighbour(rowNum,colNum,pattern,row,col)
            living = cellState(rowNum, colNum, pattern, row, col)

c           the cell is living
            if(living .EQ. 0)
     &          GOTO 0031
c             not survival
              if((num .NE. 2) .AND. (num .NE. 3)) next(row, col) = '0'
            
            GOTO 0030
c           the cell is dead
c             birth
 0031         if(num .EQ. 3) next(row, col) = '*'
c             overcrowding or loneliness 
            
	        IF(next(row,col) .NE. pattern(row,col)) changed = .TRUE.
 0030       col = col + 1
            GOTO 0022
 0023     row = row + 1
          GOTO 0021

 0024 CALL copy2DArray(next, pattern, rowNum, colNum)
c     detect for still life
      IF(changed .EQV. .FALSE.)
     &  stillLife = step - 1
       IF(changed .EQV. .FALSE.)
     &  GOTO 0029
      step = step + 1
      GOTO 0020
 0029 RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c read the content of the file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE copy2DArray(source, destination, rowNum, colNum)
      CHARACTER source(100, 80), destination(100, 80)
      INTEGER rowNum, colNum, row, col

      row = 1
 0025 IF(row .GT. rowNum)
     &    GOTO 0026
        col = 1
 0027   IF(col .GT. colNum)
     &      GOTO 0028
          destination(row, col) = source(row, col)
          col = col + 1
          GOTO 0027
 0028   row = row + 1
        GOTO 0025

 0026 RETURN
      END
	  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c return the state of a cell; -1 if index invalid, 0 if dead, 1 if alive
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER FUNCTION numLivingNeighbour(rowNum,colNum,pattern,row,col)
c local variables
      CHARACTER pattern(100, 80)
      INTEGER row, col, rowNum, colNum, cellState
      numLivingNeighbour = 0 
     &   + cellState(rowNum, colNum, pattern, row-1, col-1)
     &   + cellState(rowNum, colNum, pattern, row-1, col)
     &   + cellState(rowNum, colNum, pattern, row-1, col+1)
     &   + cellState(rowNum, colNum, pattern, row, col-1)
     &   + cellState(rowNum, colNum, pattern, row, col+1)
     &   + cellState(rowNum, colNum, pattern, row+1, col-1)
     &   + cellState(rowNum, colNum, pattern, row+1, col)
     &   + cellState(rowNum, colNum, pattern, row+1, col+1)
      RETURN
      END
	  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c return the state of a cell; 0 if index invalid, 0 if dead, 1 if alive
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER FUNCTION cellState(rowNum, colNum, pattern, row, col)
c local variables
      CHARACTER pattern(100, 80)
      INTEGER row, col, rowNum, colNum
      cellState = 0
      IF((row .LT. 1) .OR. (row .GT. rowNum)) GOTO 0009
      IF((col .LT. 1) .OR. (col .GT. colNum)) GOTO 0009
      IF(pattern(row, col) .EQ. '*') cellState = 1
      IF(pattern(row, col) .EQ. '0') cellState = 0
 0009 RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c verify if the provided argument is correct, then open the file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE writeOutput(patternName, rowNum, colNum, pattern,
     &   stillLife, numGenerations)
c local variables
      CHARACTER patternName*80, fileName*87,pattern(100,80)
      INTEGER ios, strlen, stillLife, numGenerations
      INTEGER row, col, rowNum, colNum
c
      fileName = patternName(1:strlen(patternName))//'for.txt'
      OPEN(UNIT=2, FILE=fileName, IOSTAT=ios)
      row = 1
 0011 IF(row .GT. rowNum)
     &    GOTO 0010
        col = 1
 0012   IF(col .GT. colNum)
     &      GOTO 0013
          WRITE(2, '(A, $)') pattern(row, col)
          col = col + 1
          GOTO 0012
 0013   row = row + 1
        WRITE(2, *)
        GOTO 0011
      
 0010 IF(stillLife .NE. -1) GOTO 0041
        WRITE(2, '(A, $)') 'It is still not a still life even after '
        IF(numGenerations .GE. 10000) WRITE(2, '(I5,$)') numGenerations
        IF(numGenerations .GE. 10000) GOTO 1111
        IF(numGenerations .GE. 1000) WRITE(2, '(I4,$)') numGenerations
        IF(numGenerations .GE. 1000) GOTO 1111
        IF(numGenerations .GE. 100) WRITE(2, '(I3,$)') numGenerations
        IF(numGenerations .GE. 100) GOTO 1111
        IF(numGenerations .GE. 10) WRITE(2, '(I2,$)') numGenerations
        IF(numGenerations .GE. 10) GOTO 1111
        WRITE(2, '(I1,$)') numGenerations
 1111 IF(numGenerations .EQ. 1) WRITE(2, *) 'step.'
      IF(numGenerations .GT. 1) WRITE(2, *) 'steps.'
      GOTO 0042
 0041 WRITE(2, '(A, $)') 'It is a still life'
      IF(stillLife .EQ. 0) WRITE(2, *) 'initially.'
      IF(stillLife .GT. 0) WRITE(2, '(A, $)') 'after '

        IF(stillLife .GE. 10000) WRITE(2, '(I5,$)') stillLife
        IF(stillLife .GE. 10000) GOTO 1111
        IF(stillLife .GE. 1000) WRITE(2, '(I4,$)') stillLife
        IF(stillLife .GE. 1000) GOTO 1111
        IF(stillLife .GE. 100) WRITE(2, '(I3,$)') stillLife
        IF(stillLife .GE. 100) GOTO 1111
        IF(stillLife .GE. 10) WRITE(2, '(I2,$)') stillLife
        IF(stillLife .GE. 10) GOTO 1111

      IF(stillLife .GT. 0) WRITE(2, '(A, $)') 'steps.'
 0042 CLOSE(2)
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c read the content of the file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE readFile(patternName, numGenerations, rowNum, colNum,
     &                    pattern)
c local variables
      CHARACTER patternName*80, pattern(100, 80), line*100, rowCol*6
      INTEGER numGenerations, rowNum, colNum
      INTEGER row, col, blankPos
c
      READ(1, '(A)') patternName
      READ(1, '(I)') numGenerations

	  READ(1, '(A)') rowCol
      blankPos = 1
 0051 IF(rowCol(blankPos:blankPos) .EQ. ' ') GOTO 0052
        blankPos = blankPos + 1
        GOTO 0051
 0052 READ(rowCol(1:blankPos), '(I)') rowNum
      READ(rowCol(blankPos+1:), '(I)') colNum

      row = 1
 0005 IF(row .GT. rowNum)
     &    GOTO 0006
        col = 1
        READ(1, '(A)') line
 0007   IF(col .GT. colNum)
     &      GOTO 0008
          pattern(row, col) = line(col:col)
          col = col + 1
          GOTO 0007
 0008   row = row + 1
        GOTO 0005

 0006 RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c verify if the provided argument is correct, then open the file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE openFile()
c intrinsic function declarations
      INTEGER IARGC
c local variables
      INTEGER NUMARG, IOS
      CHARACTER ARG*100
c 
      NUMARG = IARGC()
      IF(NUMARG .EQ. 1)
     &    GOTO 0001	  
        WRITE(*, *) 'ERROR: Number of arguments is not 1.'
        STOP
 0001 CALL GETARG(1, ARG)
      OPEN(UNIT=1, FILE=ARG, IOSTAT=IOS, STATUS='OLD')
      IF(IOS .EQ. 0)
     &    GOTO 0002
        WRITE(*, *) 'ERROR: File does not exist'
        STOP
 0002 RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c close the file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE closeFile()
c
      CLOSE(1)
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c count the string length
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER FUNCTION strlen(string)
      INTEGER i
      CHARACTER	string*80
      i = len(string)
 0003 IF(string(i:i) .NE. ' ')
     &    GOTO 0004
        i = i - 1
		GOTO 0003
 0004 strlen = i
      return
      end

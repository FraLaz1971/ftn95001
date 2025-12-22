		PROGRAM PRIM01
			PARAMETER(NARR=8)
			REAL XARR(NARR),YARR(NARR)
			DATA XARR/0.0,2.0,3.0,4.0,5.0,6.0,8.0,10.0/
			DATA YARR/0.0,0.8,0.5,0.14,0.8,0.8,0.35,0.9/
C specify plotting scales
			CALL SCALES(0.0,10.0,1, 0.0,1.0,1)
C start picture and draw pair of axes
			CALL AXES7('x-axis','y-axis')
C draw curve of data
			CALL BRKNCV(XARR,YARR,NARR,0)
C terminate plotting
			CALL ENDPLT
		END

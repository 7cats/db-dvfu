SELECT  tt.ID, wd.NAME , lti.BEGIN_ , lti.END_ ,
 	g.NAME, l.NAME, lt.NAME, 
        t.LAST_NAME , t.FIRST_NAME , 
        t.MIDDLE_NAME , cr.NAME
        FROM Timetable tt 
    INNER JOIN LESSONS l ON tt.LESSON_ID = l.ID 
    INNER JOIN LESSONS_TYPES lt ON tt.LESSON_TYPE_ID = lt.ID
    INNER JOIN TEACHERS t ON tt.TEACHER_ID = t.ID
    INNER JOIN GROUPS g ON tt.GROUP_ID = g.ID
    INNER JOIN Classrooms cr ON tt.CLASSROOM_ID = cr.ID
    INNER JOIN Weekdays wd ON tt.WEEKDAY_ID = wd.ID
    INNER JOIN LESSONS_TIMES lti ON tt.LESSON_TIME_ID = lti.ID
    ORDER BY tt.ID, wd.ID, lt.ID DESCENDING;
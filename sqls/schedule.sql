SELECT l.NAME AS Lesson_Name, lt.NAME AS Lesson_Type, 
       t.LAST_NAME AS Teacher_Last_Name, t.FIRST_NAME AS Teacher_First_Name, 
       t.MIDDLE_NAME AS Teacher_Middle_Name, g.NAME AS Group_Name, cr.NAME AS CLASSROOM,
       wd.NAME AS WEEKDAY, lti.BEGIN_ AS Lesson_Start, lti.END_ AS Lesson_End FROM Timetable tt 
    INNER JOIN LESSONS l ON tt.LESSON_ID = l.ID 
    INNER JOIN LESSONS_TYPES lt ON tt.LESSON_TYPE_ID = lt.ID
    INNER JOIN TEACHERS t ON tt.TEACHER_ID = t.ID
    INNER JOIN GROUPS g ON tt.GROUP_ID = g.ID
    INNER JOIN Classrooms cr ON tt.CLASSROOM_ID = cr.ID
    INNER JOIN Weekdays wd ON tt.WEEKDAY_ID = wd.ID
    INNER JOIN LESSONS_TIMES lti ON tt.LESSON_TIME_ID = lti.ID
    ORDER BY wd.ID, lt.ID DESCENDING, tt.ID
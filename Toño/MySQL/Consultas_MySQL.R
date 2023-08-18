#####Conexiones Aula####
c1 <- c('user="jvelazhe",host="50.57.15.219",port=3306,password="68iCQ2deKn",dbname="moodle_aula01")')
c2<- c('user="jvelazhe",host="198.101.170.172",port=13302,password="68iCQ2deKn",dbname="moodle_aula02")')
c3<- c('user="jvelazhe",host="198.101.170.172",port=13303,password="68iCQ2deKn",dbname="moodle_aula03")')
c4<-c('user="jvelazhe",host="50.57.15.219",port=3306,password="68iCQ2deKn",dbname="moodle_aula04")')
c5<-c('user="jvelazhe",host="198.101.170.172",port=13302,password="68iCQ2deKn",dbname="moodle_aula05")')
c6<-c('user="jvelazhe",host="198.101.170.172",port=13303,password="68iCQ2deKn",dbname="moodle_aula06")')
c7<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula07")')
c8<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula08")')
c9<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula09")')
c10<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula10")')
c12<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula12")')
c13<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula13")')
c14<-c('user="jvelazhe",host="161.47.117.126",port=3306,password="68iCQ2deKn",dbname="moodle_aula14")')


a <- "dbConnect(MySQL(),"



###Query Reporte Completo#####
queryallLic1="SELECT DISTINCT
if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,
if (mue.status = 0, 'Activo', 'Suspendido') as status,
case
when mr.id = 5 then 'UTEL'
when mr.id = 13 then 'Teleton'
end as 'institucion',
mu.id as id,
mu.username as matricula,
mu.city as 'ciudad',
mu.firstname as 'Nombre',
mu.lastname as 'Apellidos',
mu.email as 'correo',
mc.id as 'materia_id',
mc.shortname as clave,
mg.name as grupo,
cast(mgg.finalgrade as decimal(4,2))  as calificacion,
mr.shortname as rol,
mc.fullname as asignatura,\
from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as 'Fecha enrolado',
from_unixtime(mra.timemodified,'%Y-%m-%d %k:%i') as 'Fecha mdf enrol',
if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_m',
from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as 'fecha add grupo',
FROM_UNIXTIME(mc.startdate) as fecha_inicio,
FROM_UNIXTIME(mqz.timeclose) as fecha_fin,
if(mu.lastaccess is null or mu.lastaccess = 0 ,'Nunca',from_unixtime(mu.lastaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_auvi'
FROM
mdl_groups_members mgm
INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'
INNER JOIN mdl_groups mg ON mg.id = mgm.groupid and mg.name like 'Grupo%'
LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid
INNER JOIN mdl_course mc ON mc.id = mg.courseid 
INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid
INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid
INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid
inner join mdl_grade_items mgi on mgi.courseid = mc.id and itemtype = 'course'
left join mdl_grade_grades mgg on mgg.userid = mu.id and mgi.id = mgg.itemid
INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 5
LEFT JOIN mdl_quiz mqz
ON mqz.course = mc.id
AND mqz.name like '%examen final%' WHERE
mc.category IN"

queryallLic2="union
SELECT DISTINCT
if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,
if (mue.status = 0, 'Activo', 'Suspendido') as status,
case
when mr.id = 5 then 'UTEL'
when mr.id = 13 then 'Teleton'
end as 'institucion',
mu.id as id,
mu.username as matricula,
mu.city as 'ciudad',
mu.firstname as 'Nombre',
mu.lastname as 'Apellidos',
mu.email as 'correo',
mc.id as 'materia_id',
mc.shortname as clave,
mg.name as grupo,
cast(mgg.finalgrade as decimal(4,2))  as calificacion,
mr.shortname as rol,
mc.fullname as asignatura,
from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as 'Fecha enrolado',
from_unixtime(mra.timemodified,'%Y-%m-%d %k:%i') as 'Fecha mdf enrol',
if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_m',
from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as 'fecha add grupo',
FROM_UNIXTIME(mc.startdate) as fecha_inicio,
FROM_UNIXTIME(mqz.timeclose) as fecha_fin,
if(mu.lastaccess is null or mu.lastaccess = 0 ,'Nunca',from_unixtime(mu.lastaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_auvi'\
FROM
mdl_groups_members mgm\
INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'
INNER JOIN mdl_groups mg ON mg.id = mgm.groupid and mg.name like 'Grupo%'
LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid
INNER JOIN mdl_course mc ON mc.id = mg.courseid 
INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid
INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid
INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid
inner join mdl_grade_items mgi on mgi.courseid = mc.id and itemtype = 'course'
left join mdl_grade_grades mgg on mgg.userid = mu.id and mgi.id = mgg.itemid
INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 5 
LEFT JOIN mdl_quiz mqz
ON mqz.course = mc.id\
AND mqz.name like '%examen final%'
WHERE mc.category IN"



###Query Reporte Completo Mae #####
queryallmae1="SELECT DISTINCT
                     if (mu.suspended = 0, 'Activo', 'Suspendido') as
                     status_plataforma,
                     if (mue.status = 0, 'Activo', 'Suspendido') as status,
                     case
                        when mr.id = 5 then 'UTEL'
                        when mr.id = 16 then 'Teletón'
                     end as 'institucion',
                     mu.username as matricula,
                     mu.firstname as 'Nombre',
                     mu.lastname as 'Apellidos',
                      mu.email as 'correo',
                     mc.id as 'materia_id',
                     mc.shortname as clave,
                     mg.name as grupo,
                     cast(mgg.finalgrade as decimal(4,2))  as 'calificacion',
                     mr.shortname as rol,
                     mc.fullname as asignatura,
                     from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i')
                     as  'Fecha enrolado' ,
                     from_unixtime(mra.timemodified,'%Y-%m-%d %k:%i')
                     as  'Fecha mdf enrol',
                     if(mul.timeaccess is null,'Nunca',from_unixtime
                     (mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_m',
                     from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i')
                     as  'fecha add grupo',
                     if(mu.lastaccess is null or mu.lastaccess = 0,
                     'Nunca',from_unixtime(mu.lastaccess,'%Y-%m-%d %k:%i'))
                     as 'ultimo_acceso_auvi'
                     FROM
                     
                     mdl_groups_members mgm
                     INNER JOIN mdl_user mu ON mu.id = mgm.userid and
                     mu.username not like '%demo%'
                     INNER JOIN mdl_groups mg ON mg.id = mgm.groupid and
                     mg.name like 'Grupo%'
                     LEFT JOIN mdl_user_lastaccess mul ON
                     mul.userid = mgm.userid and mul.courseid = mg.courseid
                     INNER JOIN mdl_course mc ON mc.id = mg.courseid
                     INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
                     INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id
                     AND mue.userid = mgm.userid
                     INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND
                     mctx.instanceid = mg.courseid
                     INNER JOIN mdl_role_assignments mra ON
                     mra.contextid = mctx.id AND mra.userid = mgm.userid
                     inner join 
                     mdl_grade_items mgi on mgi.courseid = mc.id and itemtype = 'course' 
                     left join mdl_grade_grades mgg on mgg.userid = mu.id 
                     and mgi.id = mgg.itemid 
                     INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id =5
                     WHERE
                     mc.category in (select id from mdl_course_categories where parent in"

queryallmae2="SELECT DISTINCT
                     if (mu.suspended = 0, 'Activo', 'Suspendido') as
                     status_plataforma,
                     if (mue.status = 0, 'Activo', 'Suspendido') as status,
                     case
                        when mr.id = 5 then 'UTEL'
                        when mr.id = 16 then 'Teletón'
                     end as 'institucion',
                     mu.username as matricula,
                     mu.firstname as 'Nombre',
                     mu.lastname as 'Apellidos',
                      mu.email as 'correo',
                     mc.id as 'materia_id',
                     mc.shortname as clave,
                     mg.name as grupo,
                     cast(mgg.finalgrade as decimal(4,2))  as 'calificacion',
                     mr.shortname as rol,
                     mc.fullname as asignatura,
                     from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i')
                     as  'Fecha enrolado' ,
                     from_unixtime(mra.timemodified,'%Y-%m-%d %k:%i')
                     as  'Fecha mdf enrol',
                     if(mul.timeaccess is null,'Nunca',from_unixtime
                     (mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_m',
                     from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i')
                     as  'fecha add grupo',
                     if(mu.lastaccess is null or mu.lastaccess = 0,
                     'Nunca',from_unixtime(mu.lastaccess,'%Y-%m-%d %k:%i'))
                     as 'ultimo_acceso_auvi'
                     FROM
                     
                     mdl_groups_members mgm
                     INNER JOIN mdl_user mu ON mu.id = mgm.userid and
                     mu.username not like '%demo%'
                     INNER JOIN mdl_groups mg ON mg.id = mgm.groupid and
                     mg.name like 'Grupo%'
                     LEFT JOIN mdl_user_lastaccess mul ON
                     mul.userid = mgm.userid and mul.courseid = mg.courseid
                     INNER JOIN mdl_course mc ON mc.id = mg.courseid
                     INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
                     INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id
                     AND mue.userid = mgm.userid
                     INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND
                     mctx.instanceid = mg.courseid
                     INNER JOIN mdl_role_assignments mra ON
                     mra.contextid = mctx.id AND mra.userid = mgm.userid
                     inner join 
                     mdl_grade_items mgi on mgi.courseid = mc.id and itemtype = 'course' 
                     left join mdl_grade_grades mgg on mgg.userid = mu.id 
                     and mgi.id = mgg.itemid 
                     INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id =5
                     WHERE
                     mc.category in"

###Query Modalidad Evaluacion####
ModEva="SELECT DISTINCT
if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,
if (mue.status = 0, 'Activo', 'Suspendido') as status,
case
when mr.id = 5 then 'UTEL'
when mr.id = 13 then 'Teletón'
end as 'institucion',
mu.username as matricula,
mu.firstname as 'Nombre',
mu.lastname as 'Apellidos',
mc.shortname as clave,
mg.name as modalidad,
mr.shortname as rol,
mc.fullname as asignatura,
if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_m',
if(mu.lastaccess is null or mu.lastaccess = 0 ,'Nunca',from_unixtime(mu.lastaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso_auvi'
FROM
mdl_groups_members mgm
INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'
INNER JOIN mdl_groups mg ON mg.id = mgm.groupid and mg.name not like 'Grupo%'
LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid
INNER JOIN mdl_course mc ON mc.id = mg.courseid
INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid
INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid
INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid
INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id in (5,13)
WHERE
mc.category IN"
###Query Cal Examenes ####
querycalexa="SELECT DISTINCT
            if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,\
            if(mra.roleid= 17,'Profesor','Estudiante') as perfil,\
            mu.username as 'matricula',\
            mu.firstname as 'nombre',\
            mu.lastname as 'apellidos',\
            mc.shortname as 'ciclo',\
            substring(mc.shortname,14) as 'clave',\
            mc.fullname as 'asignatura',\
            mg.name as 'grupo',\
            mq.name as 'actividad',\
            from_unixtime(mqa.timestart) as 'comenzado',\
            if(mqa.timefinish <> 0,from_unixtime(mqa.timefinish),'') as 'completado',\
            if(mqa.timefinish <> 0,\
            concat(if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 3600),'0'), ':',\
            if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)-60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)),\
            ':',((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed))) % 60),'') as 'Requerido',\
            cast(round(((mqa.sumgrades*mq.grade)/mq.sumgrades),2) as decimal(4,2)) as 'calificacion',\
            mqa.attempt as 'intento'\
            FROM\
            mdl_course as mc\
            inner join mdl_groups mg on mg.courseid = mc.id  and mg.name like 'G%',\
            mdl_groups_members mgm,\
            mdl_quiz_attempts mqa\
            inner join mdl_quiz mq on mq.id = mqa.quiz\
            inner join mdl_user mu on mu.id = mqa.userid,\
            mdl_role_assignments as mra\
            inner join mdl_context as mctx on mra.contextid=mctx.id\
            where\
            mctx.contextlevel = 50\
            and mg.id = mgm.groupid\
            and mu.id=mra.userid\
            and mu.id = mgm.userid\
            and mc.id = mq.course\
            and mctx.instanceid=mc.id\
            and mra.roleid in (5,16,17)\
            and mc.category in"

###Query Cal Examenes Maestria #####
querycalexamae="SELECT \
                    mu.username as 'Usuario', \
                    mu.firstname as 'Nombre(s)', \
                    mu.lastname as 'Apellidos', \
                    mc.shortname as 'Ciclo', \
                    substring(mc.shortname,14) as 'Clave', \
                    mc.fullname as 'Asignatura', \
                    mg.name as 'Grupo', \
                    mq.name as 'Actividad', \
                    from_unixtime(mqa.timestart) as 'Comenzado', \
                    from_unixtime(mqa.timefinish) as 'Completado', \
                    concat( \
                    if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 3600),'0'), \
                    ':', \
                    if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)-60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)), \
                    ':', \
                    ((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed))) % 60 \
                    ) as 'Requerido', \
                    round(((mqa.sumgrades*mq.grade)/mq.sumgrades),2) as 'calificacion', \
                    mqa.attempt as 'Intento' \
                    FROM mdl_course as mc \
                    inner join mdl_groups mg on mg.courseid = mc.id, \
                    mdl_groups_members mgm, \
                    mdl_quiz_attempts mqa \
                    inner join mdl_quiz mq on mq.id = mqa.quiz \
                    inner join mdl_user mu on mu.id = mqa.userid, \
                    mdl_role_assignments as mra \
                    inner join mdl_context as mctx on mra.contextid=mctx.id \
                    where mctx.contextlevel = 50 and mg.id = mgm.groupid \
                    and mu.id=mra.userid and mu.id = mgm.userid \
                    and mc.id = mq.course and mctx.instanceid=mc.id \
                    and mra.roleid = 5 \
                    and mc.category in (select id from mdl_course_categories \
                    where parent in"
querycalexamae14="SELECT \
                    mu.username as 'Usuario', \
                    mu.firstname as 'Nombre(s)', \
                    mu.lastname as 'Apellidos', \
                    mc.shortname as 'Ciclo', \
                    substring(mc.shortname,14) as 'Clave', \
                    mc.fullname as 'Asignatura', \
                    mg.name as 'Grupo', \
                    mq.name as 'Actividad', \
                    from_unixtime(mqa.timestart) as 'Comenzado', \
                    from_unixtime(mqa.timefinish) as 'Completado', \
                    concat( \
                    if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 3600),'0'), \
                    ':', \
                    if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)-60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)), \
                    ':', \
                    ((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed))) % 60 \
                    ) as 'Requerido', \
                    round(((mqa.sumgrades*mq.grade)/mq.sumgrades),2) as 'calificacion', \
                    mqa.attempt as 'Intento' \
                    FROM mdl_course as mc \
                    inner join mdl_groups mg on mg.courseid = mc.id, \
                    mdl_groups_members mgm, \
                    mdl_quiz_attempts mqa \
                    inner join mdl_quiz mq on mq.id = mqa.quiz \
                    inner join mdl_user mu on mu.id = mqa.userid, \
                    mdl_role_assignments as mra \
                    inner join mdl_context as mctx on mra.contextid=mctx.id \
                    where mctx.contextlevel = 50 and mg.id = mgm.groupid \
                    and mu.id=mra.userid and mu.id = mgm.userid \
                    and mc.id = mq.course and mctx.instanceid=mc.id \
                    and mra.roleid = 5 \
                    and mc.category in"

###Query Foros ####
queryforo="SELECT 
mc.shortname AS clave,
mc.fullname AS asignatura,
mf.name AS foro,
mfd.name AS tema_foro,
mfp.subject AS asunto,
if (from_unixtime(mf.assesstimestart)='1969-12-31 18:00:00', '-', from_unixtime(mf.assesstimestart) ) AS inicio_valuacion,
if (from_unixtime(mf.assesstimefinish) ='1969-12-31 18:00:00', '-', from_unixtime(mf.assesstimefinish)) AS fin_valuacion,
mg.name AS grupo,
mu.username AS matricula_participante,
mra.rating as rating,
from_unixtime(mfp.created) AS fecha_evio,
if(from_unixtime(mfp.modified) = from_unixtime(mfp.created), '-',from_unixtime(mfp.modified))  AS fecha_rating,
mfp.attachment AS archivos,
mf.course AS curso,
mfd.userid AS creador_foro,
mfp.id AS id_post,
mfp.parent AS parent_post 
FROM mdl_forum mf 
LEFT JOIN mdl_forum_discussions mfd ON mfd.forum = mf.id 
LEFT JOIN mdl_forum_posts mfp ON  mfp.discussion = mfd.id
LEFT JOIN mdl_course mc ON mc.id = mf.course
LEFT JOIN mdl_groups mg ON mg.id = mfd.groupid 
LEFT JOIN mdl_user mu ON  mu.id = mfp.userid
LEFT JOIN mdl_rating mra ON mra.itemid = mfp.id
where mc.category in"
queryforo2 = "order by mc.shortname"




###Query Activities####
Queryactivities = "SELECT DISTINCT 
mu.username as matricula,
mc.shortname as clave,
mc.fullname as asignatura,
a.name as actividad,
asb.attemptnumber as intentos,
if(a.duedate != 0, from_unixtime(a.duedate,'%Y-%m-%d %k:%i'), 'Sin limite') as fechalimiteentrega,
if(asb.timecreated != 0, from_unixtime(asb.timecreated,'%Y-%m-%d %k:%i'), 'Sin envio') as fechadeenvio,
if(asb.timemodified != 0, from_unixtime(asb.timemodified,'%Y-%m-%d %k:%i'), 'Sin envio') as modificado,
if(ag.timemodified != 0, from_unixtime(ag.timemodified,'%Y-%m-%d %k:%i'), 'Sin envio') as fechacalif,
if (ag.grade is not null, ag.grade, 'Sin calificar') as calificacion, 
if (asb.status != 'new', 'Enviado para calificar', 'Sin Entrega / Visualizado') as statusentrega,
if(afc.commenttext is not null, 'Retro', 'Sin Retro') as retro,
maf.numfiles as num_archivos
from mdl_assign_grades ag 
left join mdl_assign a ON  ag.assignment = a.id
left JOIN mdl_course mc ON a.course = mc.id 
left join mdl_assign_submission asb ON ag.userid = asb.userid 
and ag.assignment=asb.assignment and ag.attemptnumber = asb.attemptnumber
left join mdl_user mu ON mu.id = ag.userid and mu.username not like '%demo%'
left join mdl_assignfeedback_comments afc on ag.id = afc.grade and ag.assignment = afc.assignment
left join mdl_assignsubmission_file maf on a.id = maf.assignment and asb.id = maf.submission
where mc.category in" 


###Query Activities Maestrias####
QueryactivitiesM14 = "SELECT DISTINCT \
        CONCAT(mc.shortname, mg.name) as clave_g,\
        mu.username as matricula,\
        mc.shortname as clave,\
        mg.name as grupo,\
        mc.fullname as asignatura,\
        a.name as actividad,\
        asb.attemptnumber as intentos,\
        if(a.duedate != 0, from_unixtime(a.duedate,'%Y-%m-%d %k:%i'), 'Sin limite') as fechalimiteentrega,\
        if(asb.timecreated != 0, from_unixtime(asb.timecreated,'%Y-%m-%d %k:%i'), 'Sin envio') as fechadeenvio,\
        if(asb.timemodified != 0, from_unixtime(asb.timemodified,'%Y-%m-%d %k:%i'), 'Sin envio') as modificado,\
        if (ag.grade is not null, ag.grade, 'Sin calificar') as calificacion, \
        if (asb.status != 'new', 'Enviado para calificar', 'Sin Entrega / Visualizado') as statusentrega,\
        if(afc.commenttext is not null, 'Retro', 'Sin Retro') as retro,\
        maf.numfiles as num_archivos\
        from mdl_assign_grades ag \
        left join mdl_assign a ON  ag.assignment = a.id\
        left JOIN mdl_course mc ON a.course = mc.id \
        left join mdl_assign_submission asb ON ag.userid = asb.userid \
        and ag.assignment=asb.assignment and ag.attemptnumber = asb.attemptnumber\
        left join mdl_user mu ON mu.id = ag.userid and mu.username not like '%demo%'\
        LEFT JOIN mdl_groups mg ON mc.id = mg.courseid and mg.name like 'Grupo%' \
        RIGHT JOIN mdl_groups_members mgm ON mgm.userid = mu.id and mgm.groupid = mg.id \
        left join mdl_assignfeedback_comments afc on ag.id = afc.grade and ag.assignment = afc.assignment\
        left join mdl_assignsubmission_file maf on a.id = maf.assignment and asb.id = maf.submission\
        WHERE \
        mc.category in"

QueryactivitiesM = "SELECT DISTINCT \
        CONCAT(mc.shortname, mg.name) as clave_g,\
        mu.username as matricula,\
        mc.shortname as clave,\
        mg.name as grupo,\
        mc.fullname as asignatura,\
        a.name as actividad,\
        asb.attemptnumber as intentos,\
        if(a.duedate != 0, from_unixtime(a.duedate,'%Y-%m-%d %k:%i'), 'Sin limite') as fechalimiteentrega,\
        if(asb.timecreated != 0, from_unixtime(asb.timecreated,'%Y-%m-%d %k:%i'), 'Sin envio') as fechadeenvio,\
        if(asb.timemodified != 0, from_unixtime(asb.timemodified,'%Y-%m-%d %k:%i'), 'Sin envio') as modificado,\
        if (ag.grade is not null, ag.grade, 'Sin calificar') as calificacion, \
        if (asb.status != 'new', 'Enviado para calificar', 'Sin Entrega / Visualizado') as statusentrega,\
        if(afc.commenttext is not null, 'Retro', 'Sin Retro') as retro,\
        maf.numfiles as num_archivos\
        from mdl_assign_grades ag \
        left join mdl_assign a ON  ag.assignment = a.id\
        left JOIN mdl_course mc ON a.course = mc.id \
        left join mdl_assign_submission asb ON ag.userid = asb.userid \
        and ag.assignment=asb.assignment and ag.attemptnumber = asb.attemptnumber\
        left join mdl_user mu ON mu.id = ag.userid and mu.username not like '%demo%'\
        LEFT JOIN mdl_groups mg ON mc.id = mg.courseid and mg.name like 'Grupo%' \
        RIGHT JOIN mdl_groups_members mgm ON mgm.userid = mu.id and mgm.groupid = mg.id \
        left join mdl_assignfeedback_comments afc on ag.id = afc.grade and ag.assignment = afc.assignment\
        left join mdl_assignsubmission_file maf on a.id = maf.assignment and asb.id = maf.submission\
        WHERE \
        mc.category in (select id from mdl_course_categories where parent in"



###Query Acc Profesores ####
queryaccprof="SELECT DISTINCT\
        if (mue.status = 0, 'Activo', 'Suspendido') as status,\
        mu.auth as auth,\
        mu.id as 'id_prof',\
        mu.username as matricula,\
        concat(mu.firstname, ' ', mu.lastname) as 'profesor',\
        mu.email as mail_profesor,\
        mc.shortname as clave,\
        mc.fullname as asignatura,\
        mg.name as grupo,\
        mr.shortname as rol,\
        if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso',\
        from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as fecha_add_grupo\
        FROM\
        mdl_groups_members mgm\
        INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'\
        INNER JOIN mdl_groups mg ON mg.id = mgm.groupid\
        LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid\
        INNER JOIN mdl_course mc ON mc.id = mg.courseid\
        INNER JOIN mdl_enrol me ON me.courseid = mg.courseid\
        INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid\
        INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid\
        INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid\
        INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 17\
        WHERE\
        mc.category in"
accprof2="and mc.visible = 1"

###Query Acc Profesores MAE ####
queryaccprofM="SELECT DISTINCT\
        if (mue.status = 0, 'Activo', 'Suspendido') as status,\
        mr.id as permiso, \
        mu.auth as auth,\
        mu.id as 'id_prof',\
        mu.username as matricula,\
        concat(mu.firstname, ' ', mu.lastname) as 'profesor',\
        mu.email as mail_profesor,\
        mc.shortname as clave,\
        mc.fullname as asignatura,\
        mg.name as grupo,\
        mr.shortname as rol,\
        if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso',\
        from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as fecha_add_grupo\
        FROM\
        mdl_groups_members mgm\
        INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'\
        INNER JOIN mdl_groups mg ON mg.id = mgm.groupid\
        LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid\
        INNER JOIN mdl_course mc ON mc.id = mg.courseid\
        INNER JOIN mdl_enrol me ON me.courseid = mg.courseid\
        INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid\
        INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid\
        INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid\
        INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 22 OR mr.id = 15\
        WHERE\
        mc.category in (SELECT id from mdl_course_categories \
                   WHERE parent in "
accprof2M="and mc.visible = 1"
queryaccprofM14="SELECT DISTINCT\
        if (mue.status = 0, 'Activo', 'Suspendido') as status,\
        mr.id as permiso, \
        mu.auth as auth,\
        mu.id as 'id_prof',\
        mu.username as matricula,\
        concat(mu.firstname, ' ', mu.lastname) as 'profesor',\
        mu.email as mail_profesor,\
        mc.shortname as clave,\
        mc.fullname as asignatura,\
        mg.name as grupo,\
        mr.shortname as rol,\
        if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso',\
        from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as fecha_add_grupo\
        FROM\
        mdl_groups_members mgm\
        INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'\
        INNER JOIN mdl_groups mg ON mg.id = mgm.groupid\
        LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid\
        INNER JOIN mdl_course mc ON mc.id = mg.courseid\
        INNER JOIN mdl_enrol me ON me.courseid = mg.courseid\
        INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid\
        INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid\
        INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid\
        INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 22 OR mr.id = 15\
        WHERE\
        mc.category in"


###Query Exa Status #####
exastat1="SELECT DISTINCT\
                mc.id, \
                mc.fullname as 'Asignatura', \
                mc.shortname as 'clave', \
                if (mc.fullname REGEXP '_D', concat(mc.shortname,'_D'), \
                        IF (mc.fullname REGEXP '_C',concat(mc.shortname,'_C'), \
                                IF(mc.fullname REGEXP '_B',concat(mc.shortname,'_B'),\
                                        mc.shortname))) AS 'clave_bloques', \
                mqz.name as 'Nombre', \
                from_unixtime(mqz.timeopen) as 'Apertura', \
                from_unixtime(mqz.timeclose) as 'Cierre', \
                mqz.attempts as 'Intentos', \
                mqtn.slotid as 'preguntas'\
                FROM mdl_quiz as mqz \
                INNER JOIN mdl_course mc on mc.category in"

exastat2="and mqz.course = mc.id \
                left join mdl_quiz_slots mqsl on mqz.id = mqsl.quizid \
                LEFT JOIN (select \
                        count(mqsl.id) as 'slotid', \
                        mqsl.quizid, \
                        mqsl.questionid, \
                        mc2.id \
                        from mdl_quiz_slots mqsl \
                        INNER JOIN  mdl_quiz mqz2 \
                        on mqz2.id = mqsl.quizid \
                        right join \
                        mdl_course mc2 on mc2.category in "
exastat3="and mqz2.course = mc2.id \
                        where quizid is not null \
                        group by mqsl.quizid \
                ) as mqtn on mqz.id = mqtn.quizid  \
                group by mc.shortname,mqz.name \
                order by mc.shortname ASC"

###Query Exa StatusM #####
exastat1M="SELECT DISTINCT\
                mc.id, \
                mc.fullname as 'Asignatura', \
                mc.shortname as 'clave', \
                if (mc.fullname REGEXP '_D', concat(mc.shortname,'_D'), \
                        IF (mc.fullname REGEXP '_C',concat(mc.shortname,'_C'), \
                                IF(mc.fullname REGEXP '_B',concat(mc.shortname,'_B'),\
                                        mc.shortname))) AS 'clave_bloques', \
                mqz.name as 'Nombre', \
                from_unixtime(mqz.timeopen) as 'Apertura', \
                from_unixtime(mqz.timeclose) as 'Cierre', \
                mqz.attempts as 'Intentos', \
                mqtn.slotid as 'preguntas'\
                FROM mdl_quiz as mqz \
                INNER JOIN mdl_course mc on mc.category in (SELECT id from mdl_course_categories \
                   WHERE parent in "

exastat2M="and mqz.course = mc.id \
                left join mdl_quiz_slots mqsl on mqz.id = mqsl.quizid \
                LEFT JOIN (select \
                        count(mqsl.id) as 'slotid', \
                        mqsl.quizid, \
                        mqsl.questionid, \
                        mc2.id \
                        from mdl_quiz_slots mqsl \
                        INNER JOIN  mdl_quiz mqz2 \
                        on mqz2.id = mqsl.quizid \
                        right join \
                        mdl_course mc2 on mc2.category in (SELECT id from mdl_course_categories \
                   WHERE parent in "
exastat3M="and mqz2.course = mc2.id \
                        where quizid is not null \
                        group by mqsl.quizid \
                ) as mqtn on mqz.id = mqtn.quizid  \
                group by mc.shortname,mqz.name \
                order by mc.shortname ASC"


exastat1="SELECT DISTINCT\
                mc.id, \
                mc.fullname as 'Asignatura', \
                mc.shortname as 'clave', \
                if (mc.fullname REGEXP '_D', concat(mc.shortname,'_D'), \
                        IF (mc.fullname REGEXP '_C',concat(mc.shortname,'_C'), \
                                IF(mc.fullname REGEXP '_B',concat(mc.shortname,'_B'),\
                                        mc.shortname))) AS 'clave_bloques', \
                mqz.name as 'Nombre', \
                from_unixtime(mqz.timeopen) as 'Apertura', \
                from_unixtime(mqz.timeclose) as 'Cierre', \
                mqz.attempts as 'Intentos', \
                mqtn.slotid as 'preguntas'\
                FROM mdl_quiz as mqz \
                INNER JOIN mdl_course mc on mc.category in"

exastat2="and mqz.course = mc.id \
                left join mdl_quiz_slots mqsl on mqz.id = mqsl.quizid \
                LEFT JOIN (select \
                        count(mqsl.id) as 'slotid', \
                        mqsl.quizid, \
                        mqsl.questionid, \
                        mc2.id \
                        from mdl_quiz_slots mqsl \
                        INNER JOIN  mdl_quiz mqz2 \
                        on mqz2.id = mqsl.quizid \
                        right join \
                        mdl_course mc2 on mc2.category in "
exastat3="and mqz2.course = mc2.id \
                        where quizid is not null \
                        group by mqsl.quizid \
                ) as mqtn on mqz.id = mqtn.quizid  \
                group by mc.shortname,mqz.name \
                order by mc.shortname ASC"


####Query acc_status ####
q_acc_status= " SELECT distinct \
        if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,\
        mu.id as 'id',\
        mu.username as 'Matricula',\
        mu.firstname as 'Nombre',\
        mu.lastname as 'Apellido',\
        mu.email as 'Email',\
        case \
            when mr.id = 5 then 'Alumno' \
            when mr.id = 17 then 'Profesor' \
            when mr.id = 3 then 'Profesor' \
            when mr.id = 4 then 'Profesor' \
            when mr.id = 20 then 'Gestor' \
            when mr.id = 18 then 'Revisor' \
            when mr.id = 9  then 'Tutor' \
            when mr.id = 13 then 'Teleton' \
            when mr.id = 16 then 'Reporteador' \
            when mr.id = 14 then 'Asesor' \
            when mr.id = 21 then 'Prof Aux' \
            else 'No definido' end as 'tipo', \
        if(mu.lastaccess=0,'Nunca',from_unixtime(mu.lastaccess)) as 'Ultimo_Acceso' \
        FROM \
        mdl_user as mu \
        left JOIN mdl_groups_members mgm \
        ON mu.id = mgm.userid \
        LEFT JOIN mdl_role_assignments mra ON mra.userid = mgm.userid \
        left JOIN mdl_role mr ON mr.id = mra.roleid \
        where \
        mu.deleted = 0 and\
        mu.username REGEXP '^[0-9]' \  
        order by \
        mr.id"






####Query correos_docentes ####
qdocentesmail="SELECT DISTINCT
        mc.fullname as asignatura,
        mc.shortname as clave,
        mm.id as id_envio,
        mr.shortname as role,
        mu.username as matricula,
        mu.firstname as nombre,
        mu.lastname as apellido,
        mmu.role as Tipo,
        from_unixtime(mm.time,'%Y-%m-%d %k:%i') as Fecha_Envio,
        mm.normalizedsubject as Titulo,
        if (mmu.unread =  0, 'Abierto', 'Sin_Abrir') as status_lectura
        FROM
        mdl_local_mail_messages mm
        INNER JOIN mdl_course mc ON mc.id = mm.courseid
		INNER JOIN mdl_local_mail_message_users mmu ON mmu.messageid = mm.id
        INNER JOIN mdl_user mu ON mu.id = mmu.userid
        INNER JOIN mdl_role_assignments mra ON mra.userid = mmu.userid 
        INNER JOIN mdl_role mr ON mr.id = mra.roleid
        WHERE
        mc.category in"

####Query correos_docentes Posgrados ####
qdocentesmailp="SELECT DISTINCT
        mc.fullname as asignatura,
        mc.shortname as clave,
        mm.id as id_envio,
        mr.shortname as role,
        mu.username as matricula,
        mu.firstname as nombre,
        mu.lastname as apellido,
        mmu.role as Tipo,
        from_unixtime(mm.time,'%Y-%m-%d %k:%i') as Fecha_Envio,
        mm.normalizedsubject as Titulo,
        if (mmu.unread =  0, 'Abierto', 'Sin_Abrir') as status_lectura
        FROM
        mdl_local_mail_messages mm
        INNER JOIN mdl_course mc ON mc.id = mm.courseid
		INNER JOIN mdl_local_mail_message_users mmu ON mmu.messageid = mm.id
        INNER JOIN mdl_user mu ON mu.id = mmu.userid
        INNER JOIN mdl_role_assignments mra ON mra.userid = mmu.userid 
        INNER JOIN mdl_role mr ON mr.id = mra.roleid
        WHERE
        mc.category in(select id from mdl_course_categories where parent in"











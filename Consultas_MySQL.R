library(MultiLEDS); library(dplyr)
diremail("C:/Users/jsalinba/Documents/Reportes","jsalinba@utel.edu.mx") #"jsalinba","8Xo6KzNzuJAWNc5"     "jvelazhe","68iCQ2deKn"
#####--------Transformando vectores--------#####
Rep <- leer(c("Control descargas","gsheet"),sheet="Aulas Utel") %>% mutate(Aulas=as.integer(Aulas)) %>% arrange(Aulas) %>%
  mutate(Aulas=as.character(Aulas),Aulas=if_else(nchar(Aulas)==1,paste("0",Aulas,sep=""),Aulas),
         Aulas=paste("moodle_aula",Aulas,sep=""),IDs=paste("(",IDs,")",sep=""),
         Conexiones=paste("diremail(msql = c(",Conexiones,",",'"jvelazhe","68iCQ2deKn",','"',Aulas,'"))',sep=""))
#####--------Creando lista--------#####
Aulas <- list()
mods <- Rep$Modalidad[!duplicated(Rep$Modalidad)]#Modalidades
for (i in 1:length(mods)) {
  querys <- c(); ModsEva <- c()#Iniciando vectores vacíos
  Rep2 <- filter(Rep,Modalidad==mods[i])#Filtrando por cada modalidad
  for (j in 1:length(Rep2$IDs)) {
    #Conexiones
    Conx <- Rep2[[j,"Conexiones"]]
    #Querys Licenciatura y Maestría (examenes, actividades, profesores, exa status y correos docentes)
    if (mods[i]=="Licenciatura") {
      #Reporte completo
      query <- paste("SELECT DISTINCT
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
      mc.category IN ",Rep2[[j,"IDs"]],
      "union
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
      AND mqz.name like '%examen final%'
      WHERE mc.category IN ",Rep2[[j,"IDs"]],sep="")
      #Calificaciones examenes
      CalExa <- paste("SELECT DISTINCT
      if (mu.suspended = 0, 'Activo', 'Suspendido') as status_plataforma,
      if(mra.roleid= 17,'Profesor','Estudiante') as perfil,
      mu.username as 'matricula',
      mu.firstname as 'nombre',
      mu.lastname as 'apellidos',
      mc.shortname as 'ciclo',
      substring(mc.shortname,14) as 'clave',
      mc.fullname as 'asignatura',
      mg.name as 'grupo',
      mq.name as 'actividad',
      from_unixtime(mqa.timestart) as 'comenzado',
      if(mqa.timefinish <> 0,from_unixtime(mqa.timefinish),'') as 'completado',
      if(mqa.timefinish <> 0,
      concat(if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 3600),'0'), ':',
      if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)-60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)),
      ':',((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed))) % 60),'') as 'Requerido',
      cast(round(((mqa.sumgrades*mq.grade)/mq.sumgrades),2) as decimal(4,2)) as 'calificacion',
      mqa.attempt as 'intento'
      FROM
      mdl_course as mc
      inner join mdl_groups mg on mg.courseid = mc.id  and mg.name like 'G%',
      mdl_groups_members mgm,
      mdl_quiz_attempts mqa
      inner join mdl_quiz mq on mq.id = mqa.quiz
      inner join mdl_user mu on mu.id = mqa.userid,
      mdl_role_assignments as mra
      inner join mdl_context as mctx on mra.contextid=mctx.id
      where
      mctx.contextlevel = 50
      and mg.id = mgm.groupid
      and mu.id=mra.userid
      and mu.id = mgm.userid
      and mc.id = mq.course
      and mctx.instanceid=mc.id
      and mra.roleid in (5,16,17)
      and mc.category IN ",Rep2[[j,"IDs"]],sep="")
      #Calificaciones actividades
      CalAct <- paste("SELECT DISTINCT 
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
      where mc.category IN ",Rep2[[j,"IDs"]],sep="")
      
      #Profesores
      Prof <- paste("SELECT DISTINCT
      if (mue.status = 0, 'Activo', 'Suspendido') as status,
      mu.auth as auth,
      mu.id as 'id_prof',
      mu.username as matricula,
      concat(mu.firstname, ' ', mu.lastname) as 'profesor',
      mu.email as mail_profesor,
      mc.shortname as clave,
      mc.fullname as asignatura,
      mg.name as grupo,
      mr.shortname as rol,
      if(mul.timeaccess is null,'Nunca',from_unixtime(mul.timeaccess,'%Y-%m-%d %k:%i')) as 'ultimo_acceso',
      from_unixtime(mgm.timeadded,'%Y-%m-%d %k:%i') as fecha_add_grupo
      FROM
      mdl_groups_members mgm
      INNER JOIN mdl_user mu ON mu.id = mgm.userid and mu.username not like '%demo%'
      INNER JOIN mdl_groups mg ON mg.id = mgm.groupid
      LEFT JOIN mdl_user_lastaccess mul ON mul.userid = mgm.userid and mul.courseid = mg.courseid
      INNER JOIN mdl_course mc ON mc.id = mg.courseid
      INNER JOIN mdl_enrol me ON me.courseid = mg.courseid
      INNER JOIN mdl_user_enrolments mue ON mue.enrolid = me.id AND mue.userid = mgm.userid
      INNER JOIN mdl_context mctx ON mctx.contextlevel = 50 AND mctx.instanceid = mg.courseid
      INNER JOIN mdl_role_assignments mra ON mra.contextid = mctx.id AND mra.userid = mgm.userid
      INNER JOIN mdl_role mr ON mr.id = mra.roleid and mr.id = 17
      WHERE
      mc.category IN ",Rep2[[j,"IDs"]],sep="")
      
      #Exa status
      ExaSta <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      #Correos docentes
      CorDoc <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }else{
      query <- paste("SELECT DISTINCT
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
      mc.category IN ",Rep2[[j,"IDs"]],sep="")
      #Calificaciones examenes
      CalExa <- paste("SELECT 
      mu.username as 'Usuario', 
      mu.firstname as 'Nombre(s)', 
      mu.lastname as 'Apellidos', 
      mc.shortname as 'Ciclo', 
      substring(mc.shortname,14) as 'Clave', 
      mc.fullname as 'Asignatura', 
      mg.name as 'Grupo', 
      mq.name as 'Actividad', 
      from_unixtime(mqa.timestart) as 'Comenzado', 
      from_unixtime(mqa.timefinish) as 'Completado', 
      concat( 
      if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 3600),'0'), 
      ':', 
      if(((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)>60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)-60,((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed)) div 60)), 
      ':', 
      ((cast(mqa.timefinish as signed)-cast(mqa.timestart as signed))) % 60 
      ) as 'Requerido', 
      round(((mqa.sumgrades*mq.grade)/mq.sumgrades),2) as 'calificacion', 
      mqa.attempt as 'Intento' 
      FROM mdl_course as mc 
      inner join mdl_groups mg on mg.courseid = mc.id, 
      mdl_groups_members mgm, 
      mdl_quiz_attempts mqa 
      inner join mdl_quiz mq on mq.id = mqa.quiz 
      inner join mdl_user mu on mu.id = mqa.userid, 
      mdl_role_assignments as mra 
      inner join mdl_context as mctx on mra.contextid=mctx.id 
      where mctx.contextlevel = 50 and mg.id = mgm.groupid 
      and mu.id=mra.userid and mu.id = mgm.userid 
      and mc.id = mq.course and mctx.instanceid=mc.id 
      and mra.roleid = 5 
      and mc.category in (select id from mdl_course_categories 
      where parent IN ",Rep2[[j,"IDs"]],sep="")
      
      #Calificaciones actividades
      CalAct <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      #Profesores
      Prof <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      #Exa status
      ExaSta <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      #Correos docentes
      CorDoc <- paste("Algo",Rep2[[j,"IDs"]],sep="")
      
      
      
    }
    #Modalidad de evaluación
    ModEva <- paste("SELECT DISTINCT
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
    mc.category IN ",Rep2[[j,"IDs"]],sep="")
    
    
    
    
    
    eval(parse(text = paste("querys <- c(querys,",Rep2[[j,"Aulas"]],"=query,",Rep2[[j,"Aulas"]],"_ModEva=ModEva,",Rep2[[j,"Aulas"]],"_Conx=Conx,",
                            Rep2[[j,"Aulas"]],"_CalExa=CalExa,",Rep2[[j,"Aulas"]],"_CalAct=CalAct,",Rep2[[j,"Aulas"]],"_Prof=Prof,",
                            Rep2[[j,"Aulas"]],"_ExaSta=ExaSta,",Rep2[[j,"Aulas"]],"_CorDoc=CorDoc)",sep = "")))
  }
  Aulas <- c(Aulas,list(querys)); names(Aulas)[i] <- mods[i]
}
remove("ModEva","ModsEva","CalExa","CalAct","Prof","ExaSta","CorDoc","mods","query","querys","Rep2")
















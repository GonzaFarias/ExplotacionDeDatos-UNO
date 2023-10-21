
###############################################
# Tema: Conexion a bases de datos.
# clase: 04
# Fecha: 02/09/2022  
# version actual: 02/09/2022
# Autor: Mendoza, Dante.
###############################################

# este script ejemplifica como conectarnos a una BD postgreSQL y 
# como grabar y leer tablas.
# Crear la BD en postgreSQL antes de continuar, aqui la llamamos 'taller_r'

##### PREPARACION #####
# creo un data frame de ejemplo para guardar en nuestra BD
# Puede incluso ser el resultado de algun analisis y normalizado previo.
mi_df <- data.frame(
  "id" = 1:3, 
  "factor" = c("a", "b", "c"), 
  "numero" = c(1.2, 2.2, 3.3),
  "cadena" = as.character(c("a", "b", "c"))
)

head(mi_df) # Informacion del DF.

# Importamos las bibliotecas a utilizar.
library('RPostgreSQL')
library(DBI)

##### CONEXIONES #####
# Establezco la conexion con la base de datos PoststgreSQL RPostgreSQL:
pg = dbDriver("PostgreSQL") # version (localhost como default)

con = dbConnect(pg, user="postgres", password="admin",
                host="localhost", port=5432, dbname= 'taller_r')

##### eJECUCION #####
# requiere package RSQLite
library(RSQLite) # para funcion dbWriteTable
dbGetQuery(con, "DROP TABLE IF EXISTS mi_df" ) # para borrar la version anterior de la tabla.
dbWriteTable(con,'mi_df', mi_df, row.names=FALSE) # Pasamos el DF que deseamos guardar.

# Commit los cambios:
dbCommit(con)

# Para ejecutar o leer de una tabla:
query1 = dbGetQuery(con, "select * from mi_df" )
query2 = dbGetQuery(con, "select * from mi_df where id = 1" )

# Cierro PostgreSQL conexion. 
dbDisconnect(con)

# Sitio de interes:
# https://www.r-bloggers.com/r-and-postgresql-using-rpostgresql-and-sqldf/



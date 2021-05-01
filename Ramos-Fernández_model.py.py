# -*- coding: cp1252 -*-                                             
import random
import math
import os
import time


#####################################################################################
#####################################################################################
#############-----------inizializa las variables basicas----------###################

# este es el nombre de la carpeta de los datos
carpetaPrincipal = "Mem.Limit.datos3"

#el rango de valores beta (exponente) del bosque
betas = [1.5, 2.5, 3.5, 4.5]

#rango de poblaciones de monos(se prueva con todos los betas)
monos = [100]

# numero de archivos que se crean por programa con diferentes condiciones inicilaes
# para obtener los promedios
archivos = 50

#numero de pasos de una corrida
iteraciones = 100

# numero de arboles por corrida
arboles = 50000

## numero de memora para cada mono
#recuerda que la memoria > iteraciones y  memoria <= arboles o aparece un error
memoria = 25000 

#####################################################################################
#####################################################################################

class Tablero:
    """Esta clase representa el tablero de juego en donde los monos y los
    arboles interactuan"""

    def __init__(self, monos, memoria, arboles, beta, kmax):
        """este es el constructor del tablero y es llamado cada ves
        que se crea un nuevo tablero, con un numero de monos y un numero de
        arboles"""

        print "tablero creado"
        ## inicializa variables
        self.numMonos = monos
        self.memoria = memoria       
        self.numArboles = arboles
        self.beta = beta
        self.kmax = kmax
        #self.file = file
        self.bolsaArboles = []
        self.bolsaMonos = []
        
        ##crea los arboles
        self.creaArboles()
        
        ##crea los monos
        self.creaMonos()
        #----------------------------------------------------------------------------------------------------------

    def siguienteEstado(self):
        """esta funcion se encarga de llamar al cambio al nuevo
        estado a los monos y a los arboles"""
        z = 0
        # el loop de siguiente estado en arboles
        while z < self.numArboles:
            self.bolsaArboles[z].siguienteEstado()
            z = z + 1
        z = 0
        # el loop de siguiente estado de monos
        while z < self.numMonos:
            self.bolsaMonos[z].siguienteEstado()
            z = z + 1
        z = 0
        string = ""
        while z < len(self.bolsaMonos):
            string = string + str(self.bolsaMonos[z].nombre)+","+ str(self.bolsaMonos[z].x)+","+str(self.bolsaMonos[z].y)+","+str(self.bolsaMonos[z].arbol.k)+","
            z = z + 1
        return string+"\n"
        #----------------------------------------------------------------------------------------------------------  
            
    def creaArboles(self):
        """este metodo se encarga de la creacion de los arboles segun una
        distribucion beta, en sus k´s """
        print "tablero creando "+str(self.numArboles)+"  arboles"
        z = 1
        c = 0
        arbol = 0 # contador de arboles creados
        #para la variable c que es igual a la suma de las p(k)´s de k=1 a k= kmax
        #es necesario iterar
        while z <= self.kmax:
            c = c + pow(z, -self.beta)
            z = z +1
            
        # el loop de creacion
        # que se repite hasta que se creeen todos los arboles
        while len(self.bolsaArboles) < self.numArboles:
            k = 1
            #intenta crear un arbol para toda las distribuciones de k -- kmax
            #problema con k = 1
            while (k <= self.kmax)&( len(self.bolsaArboles) < self.numArboles):
                c = c + pow(k, -self.beta)
                pk = pow(k, -self.beta)/c
                z = 0
                #intenta un numero de veces crear un arbol de una sierta k
                while (z <self.numArboles )&( len(self.bolsaArboles) < self.numArboles) :
                    z = z + 1
                    azar = random.random()
                    #si la probavilidad se da entonces crealo
                    if azar < pk:
                        self.bolsaArboles.append(Arbol(random.random(),random.random(), k, arbol))
                        arbol = arbol + 1
                k = k + 1
    #----------------------------------------------------------------------------------------------------------
        
    def creaMonos(self):
        """este metodo crea una cierta cantidad de monos distribuidos
        al azar y una cordenadas x,y y una k de memorias"""
        print "tablero creando "+str(self.numMonos)+" monos"
        z = 0
        # el loop de ceaccion
        while z < self.numMonos :
            self.bolsaMonos.append(Mono( z , self.dameArbolesMemoria(), self))
            z = z + 1
        #----------------------------------------------------------------------------------------------------------

    def dameArbolesMemoria(self):
        "este metodo genera y regresa un conjunto de arboles de memoria para"
        "un mono"
        #llena un paquete del tamaño de self.memoria
        recuerdos = []
        #obten una copia de la bolsa de arboles a la cual puedas quitarle los arboles escojidos
        bolsaArboles = self.bolsaArboles[:]
        while len(recuerdos)  < self.memoria:
            #escoje un arbol al azar
            azar = random.randint(0 , (len(bolsaArboles)-1))
            #vuelvelo memoria
            recuerdos.append((bolsaArboles[azar].x,bolsaArboles[azar].y,bolsaArboles[azar].k))
            #quitalo de la bolsa de arboles
            del bolsaArboles[azar]
            

        return recuerdos
    
    def dameArbolEnPos(self, x, y):
        "este metodo funciona para localizar un objeto arbol en base a sus cordenadas"
        "x, y y una k"
        
        z=0
        #busca comparando los arboles de la bolsa con las cordenadas dadas
        while z < len(self.bolsaArboles):
            if (self.bolsaArboles[z].x == x) & (self.bolsaArboles[z].y == y):
                return self.bolsaArboles[z]
            z = z + 1
        #----------------------------------------------------------------------------------------------------------
       ##############################____TERMINA CLASE TABLERO___ ##############################

        
class Mono:
    """Esta clase representa a un mono con todas sus funciones
    y atributos"""

    def __init__(self, nombre, recuerdos, tablero):
        """crea un mono en una posicion, con un nombre y una memoria"""
        
        ## inicializa variables
        self.nombre = nombre
        self.recuerdos = recuerdos
        self.tablero = tablero
        azar = random.randint(0, (len(self.recuerdos) - 1 ))
        self.arbol = self.tablero.dameArbolEnPos(self.recuerdos[azar][0],self.recuerdos[azar][1])
        self.x = self.arbol.x
        self.y = self.arbol.y
        self.quitaRecuerdo(self.arbol)
        self.arbol.ponHuesped()
        #----------------------------------------------------------------------------------------------------------

    def siguienteEstado(self):
        """en este metodo el mono se queda o busca mas comida"""
        #si la comida se acabo te mueves
        if self.arbol.k <= 0 :
            #tasa las diferentes distancias y k´s
            arbol = self.escojeArbol()
            #cambia cordenadas
            self.x = arbol[0]
            self.y = arbol[1]
            
            #salte del viejo arbol y ponte en el nuevo 
            arbol = self.tablero.dameArbolEnPos(self.x , self.y)
            self.arbol.quitaHuesped()
            arbol.ponHuesped()
            self.quitaRecuerdo(arbol)
            self.arbol = arbol
        #----------------------------------------------------------------------------------------------------------
        
    def quitaRecuerdo(self, arbol):
        """este metodo se encarga de quitar un recuerdo"""
        z = 0
        #busca las cordenadas del arbol para quitar las coordenadas recuerdo 
        
        while z < len(self.recuerdos):
            if (arbol.x == self.recuerdos[z][0])&(arbol.y == self.recuerdos[z][1]):
                del self.recuerdos[z]
                break
            z = z + 1
        #----------------------------------------------------------------------------------------------------------
        
    def escojeArbol(self):
        """Este metodo sirve para seleccionar un arbol dentro
        de la memoria incompleta de este mono en particular"""
        z = 0
        #agarra el primer recuerdo calculando para ver cual esta mas serca en comparacion
        muestra = self.recuerdos[0]
        distanciaMuestra = math.sqrt(pow(self.x - muestra[0],2) + pow(self.y - muestra[1],2))
        lkMuestra = distanciaMuestra/muestra[2]
        #compara todos
        while z < len(self.recuerdos) :
            distanciaZ =  math.sqrt(pow(self.x - self.recuerdos[z][0],2) + pow(self.y - self.recuerdos[z][1],2))
            lkZ = distanciaZ/self.recuerdos[z][2]
            #si encuentras una distancia mas chica que la actual cambialo
            if lkZ < lkMuestra :
                muestra = self.recuerdos[z]
                distanciaMuestra = math.sqrt(pow(self.x - muestra[0],2) + pow(self.y - muestra[1],2))
                lkMuestra = distanciaMuestra/muestra[2]
            z = z + 1        
        return muestra
        #----------------------------------------------------------------------------------------------------------
    
   ##############################____TERMINA CLASE MONO___ ##############################
            
class Arbol:
    """Esta clase representa a un arbol con todas sus funciones y
    atributos"""
    
    ##crea un arbol de un cierto tamaño k, una posicion y un nombre 
    def __init__(self, x, y, k, nombre):
        """este metodo se llama cuando creas un nuevo arbol"""

        #inicializa las variables
        self.x = x
        self.y =y
        self.k =k
        self.nombre = nombre
        self.ocupantes = 0
        #----------------------------------------------------------------------------------------------------------
        
    def siguienteEstado(self):
        """esta funcion calcula el siguiente estado del arbol en base a cambiar su k"""
        if self.ocupantes > 0:
            self.quitaFrutas()
        #----------------------------------------------------------------------------------------------------------
        
    def ponHuesped(self):
        """este metodo se usa cuando pones un ocupante mas """
        self.ocupantes = self.ocupantes + 1
        #----------------------------------------------------------------------------------------------------------
        
    def quitaHuesped(self):
        """este metodo es llamado para quitar un huesped"""
        self.ocupantes = self.ocupantes - 1
        #----------------------------------------------------------------------------------------------------------
        
    def quitaFrutas(self):
        """Esta metod es utilizado por el arbol para descontar
        las frutas comidas por los monos"""
        if self.k > 0:
            self.k = self.k - self.ocupantes
        #----------------------------------------------------------------------------------------------------------
        
   ##############################____TERMINA CLASE ARBOL___ ##############################





#################################################################################################
#-----------------------------------COMIENZA EL PROGRAMA---------------------------------------##
#-------------------------------------------MAIN()---------------------------------------------##
#################################################################################################

def main():
    """esta es la funcion principal donde comienza la ejecucion del
    programa
    """
    
    # crea el directorio donde guardas todos los datos
    os.mkdir(carpetaPrincipal)
    
    #pon el beta inicial e itera programas hasta un beta maximo

    for beta in betas :

        # crea un subdirectorio para cada uno de los betas
        # carpetaPrincipal/beta<N#i>       
        os.mkdir(carpetaPrincipal+"/beta"+str(beta))

        #por cada beta provado obten un kmax
        kmax = dameKmax(arboles, beta)
        
        #para todo beta prueva una variedad de cantidades de monos
        #desde uno inicial hasta un tope
        for mono in monos:

            # crea un subdirectorio para cada una de las diferentes cantidades de monos 
            # carpetaPrincipal/beta<N#>/monos<N#> 
            os.mkdir(carpetaPrincipal+"/beta"+str(beta)+"/monos"+str(mono))

            #por cada una de las corridas de un cierto beta y un cierto numero de monos
            #corre un numero de veces el programa con las mismas condiciones iniciales
            z = 0
            while z < archivos:
                print "beta = "+str(beta)+" monos = "+str(mono)+" corrida = "+str(z)
                f=open(carpetaPrincipal+"/beta"+str(beta)+"/monos"+str(mono)+"/"+str(z)+".txt", 'w')

                ###############----aqui se ejecuta todo el programa-----#############
                print "---- pinche tablero"
                tablero = Tablero(mono, memoria, arboles, beta, kmax)
                print "----------------------------------------"
                iteracion = 0
                # el ciclo principal de iteracion
                while iteracion < iteraciones:
                    # llama a la funcion de siguiente estado en el tablero y el llama a la de los
                    # monos y arboles
                    f.write( tablero.siguienteEstado() )
                    iteracion = iteracion + 1
                f.close()
                ###########################################################################
                z = z+1
            ####--una ves creados los archivos analizalos------------------------
            path = carpetaPrincipal+"/beta"+str(beta)+"/monos"+str(mono)+"/"
            inicia(iteraciones, mono, archivos, beta,path)
    #----------------------------------------------------------------------------------------------------------

def dameKmax(numero_de_arboles, beta):
    """kmax representa el arbol con mas frutas posible, y este metodo
    se encarga de generar una distribucion correcta en as a un buen kmax"""
    #inicializa las variables
    z = 1
    norm = 0
    pkmax = 0
    # busca Kmax hasta encontrarla
    while True:      
        norm = norm + pow(z , -beta)
        pkmax = numero_de_arboles * pow(z , -beta)/norm        
        if pkmax  <= 1.0 :
            print "kmax = "+str(z)
            return z 
        z =  z + 1
        #----------------------------------------------------------------------------------------------------------      
############################################################################################
        ##########------aca comiensan las funciones de analisis---------------#########
def analiza(iteraciones, monos, archivo):
    "este metodo te regresa tres arreglos de distribucion"    
    f = archivo
    l = []
    z = 0
    #lee todo el archivo y crea un array de lineas del archivo
    while z < iteraciones:
        l.append(f.readline())
        z = z + 1
    
    ll = []    
    #parte cada una de la lineas separando los numeros de cada una 
    for reglon in l:
        ll.append(reglon.split(","))

    l = []
    lll = []

    #agrupa los numeros en grupo de 4->> (numero_de_mono, pos_x, pos_y, k_arbol)
    for reglon in ll:
        z = 0
        num = 0
        while z < monos:
            lll.append((reglon[num], reglon[num+1], reglon[num+2], reglon[num+3]))
            num = num + 4
            z = z + 1
        l.append(lll)
        lll = []

    #---------------------------------------------------------------------------
    #define los grupos de cada linea
    greglon = []
    num = 0
    #checa todos los reglones 
    while num < len(l):
        grupos = []
        #mientras la longitud del reglon sea mallor que cero checa los grupos y luego
        #quitalos de la lista
        while len(l[num]) > 0:
            #si el reglon es tiene dos monos o mas
            if len(l[num])>= 2:
                #compara el mono [0] con los demas para forma un grupo
                grupo = []
                compara = l[num][0]
                #checa a todos los monos empezando por el ultimo
                z = len(l[num]) - 1 
                while z >= 0 :
                    #checa si las cordenadas x, y, y el k son iguales pero es otro mono
                    # es decir su nombre es otro
                    if (compara[1] == l[num][z][1]):
                        if(compara[2]==l[num][z][2]):
                            if(compara[0]!= l[num][z][0]):
                                # si todo lo anterior era sierto pon al mono en un grupo
                                #y quitalo del reglon
                                grupo.append(l[num][z][0])
                                del l[num][z]
                    z = z - 1
                #al final pon a compara con el grupo aunque este este vacio
                grupo.append(compara[0])
                del l[num][0]
                #pon el grupo con los demas grupos
                grupos.append(grupo)
            else:#en caso que el reglon solo tenga un mono
                grupo = []
                grupo.append(l[num][0][0])
                grupos.append(grupo)
                del l[num][0]
        ##agrega los grupos como un reglon
        greglon.append(grupos)
        num = num + 1

    #cierra el archivo
    f.close()
    
    #crea la distribucion para contar
    distribucion = []
    z = 1
    while z <= monos:
        distribucion.append((z , 0))
        z = z + 1
    rdistribucion = distribucion[:]
    #cuenta los tamaños de cada grupo
    for grupo in grupos:
        a = len(grupo)
        b = distribucion[len(grupo)-1][1]
        distribucion[len(grupo)-1] = (a, b+1)
        
    #cuenta los tamaños de cada grupo en cada reglon
    for reglon in greglon:
        for grupo in reglon:
            a = len(grupo)
            b = rdistribucion[len(grupo)-1][1]
            rdistribucion[len(grupo)-1] = (a, b+1)   

    ##crea la distribucionde duraciones
    duracion = []
    z = 1
    while z <= monos:
        dura = []
        zz = 1
        while zz <= iteraciones:
            dura.append((z , zz, 0))
            zz = zz + 1
        duracion.append(dura[:])
        z = z + 1    
    ##checa cuanto duran los grupos 
    z = 0
    gsobreviven = []
    #checa todos los reglones
    while z < len(greglon):
        #si es el primer reglon
        if z == 0:
            #agregalos a la lista de sobrevivientes para compararlos el tiempo que sigue
            zz = len(greglon[z]) - 1
            while zz >= 0:
                #al ponerlos pon (el grupo, su duracion)
                gsobreviven.append((greglon[z][zz], 1))
                #quita los grupos del reglon
                del greglon[z][zz]
                zz = zz - 1
        
        else:#si no es el primer reglon
            #checa los grupos viejos
            zz = len(gsobreviven)-1
            while zz >= 0:
                # a cada gupo viejo checa con los grupos del reglon a ver si sobrevivieron
                sobrevivi = 0
                zzz = len(greglon[z])-1
                while zzz >= 0:
                    #si sobrevivieron saca el grupo del reglon y aumenta la cuenta
                    #del grupo en los sobrevivientes
                    if greglon[z][zzz] == gsobreviven[zz][0]:
                        sobrevivi = 1
                        gsobreviven[zz] = (gsobreviven[zz][0], gsobreviven[zz][1]+1)
                        del greglon[z][zzz]
                        break
                    zzz = zzz -1
                #si no sobrevivieron 
                if not sobrevivi:
                    #cambia el valor de la distribucion]
                    duracion[len(gsobreviven[zz][0])-1][gsobreviven[zz][1]-1] = (len(gsobreviven[zz][0]), gsobreviven[zz][1], duracion[len(gsobreviven[zz][0])][gsobreviven[zz][1]][2]+1)
                    #quitalo de los sobreviviente
                    del gsobreviven[zz]                                 
                zz = zz - 1
            #si el reglon contiene a algien es un nuevo grupo
            if len(greglon[z])> 0:
                zzz = len(greglon[z]) - 1
                while zzz >= 0:
                    gsobreviven.append((greglon[z][zzz], 1))
                    del greglon[z][zzz]
                    zzz = zzz - 1
        z = z+1       
    return distribucion, rdistribucion, duracion


def inicia(iteraciones, monos, archivos, beta,  path):

    
    leyendo = []
    z = 0
    while z < archivos:
        f=open(str(path)+"/"+str(z)+".txt", 'r')
        leyendo.append(f)
        z =  z + 1
    
    fdis=open(path+str(beta)+"-"+str(monos)+"-grupos_vs_tamaño.txt", 'w')
    fdur = open(path+str(beta)+"-"+str(monos)+"-duracion_vs_tamaño.txt", 'w')
    freglon= open(path+str(beta)+"-"+str(monos)+"-grupos_reglon_vs_tamaño.txt", 'w')
    #lee los archivos de distri, rdistri, y duracion
    distribuciones = []
    rdistribuciones = []
    duraciones = []
    for archivo in leyendo:
    
        a1, a2, a3 = analiza(iteraciones, monos, archivo)
        a = [a1, a2, a3]
        distribuciones.append(a[0])
        rdistribuciones.append(a[1])
        duraciones.append(a[2])

    #crea los promedios de la distribucion de grupos
    #crea la distribucion para contar
        distribucion = []
        z = 1
        while z <= monos:
            distribucion.append((z , 0))
            z = z + 1
        rdistribucion = distribucion[:]
    
    ##crea la distribucionde duraciones
        duracion = []
        z = 1
        while z <= monos:
            dura = []
            zz = 1
            while zz <= iteraciones:
                dura.append((z , zz, 0))
                zz = zz + 1
            duracion.append(dura[:])
            z = z + 1

    #cuenta los tamaños de cada grupo
    z = 0
    while z < len(distribuciones):
        for dis in distribuciones[z]:
            distribucion[dis[0]-1] = (dis[0], distribucion[dis[0]- 1][1] + dis[1] )
        z =  z + 1

    z = 0
    while z < len(rdistribuciones):
        #cuenta los tamaños de cada grupo en cada reglon
        for rdis in rdistribuciones[z]:
            rdistribucion[rdis[0]-1] = (rdis[0], rdistribucion[rdis[0]- 1][1] + rdis[1] )
        z = z + 1
    
    z = 0
    while z < len(duraciones):
        ##cuenta los grupos segun duraron    
        for dur in duraciones[z]:
            for mas in dur:
                duracion[mas[0]-1][mas[1]-1] = (mas[0],mas[1], duracion[mas[0]-1][mas[1]-1][2] + mas[2] )
        z = z + 1
    
    ##crea el archivo de distribucion
    for data in distribucion:
        fdis.write(str(data[0])+","+str(data[1])+"\n")
    fdis.close()
    ##crea el archivo de duracion
    ##primero pon los monos en el eje de las x's
    z = monos - 1
    while z >= 0 :
        #luego pon la duracion en el eje de las y's    
        zz = 0
        while zz < iteraciones :
            fdur.write(str(duracion[z][zz][2])+",")
            zz = zz + 1
        fdur.write("\n")
        z =  z - 1
    
    fdur.close()
    ##crea el archivo de distribuciones por reglon
    for data in rdistribucion:
        freglon.write(str(data[0])+","+str(data[1])+"\n")
    freglon.close()

        
# si este modulo tiene un main() llamalo        
if __name__ == '__main__': main()

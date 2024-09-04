--- PASOS INICIALES ---
-- Crear la tabla persistence utilizando el tipo definido en el paquete mapa

CREATE OR REPLACE TYPE row_type IS VARRAY(10) OF VARCHAR2(10);
/
CREATE OR REPLACE TYPE matrix_type IS VARRAY(10) OF row_type;
/

--------- CREAR MAPA --------

CREATE OR REPLACE PACKAGE mapa AS
    -- Procedimiento para inicializar la matriz
    PROCEDURE init_matrix(out_matrix OUT matrix_type);
    
    -- Procedimiento para mostrar la matriz
    PROCEDURE display_matrix(matrix IN matrix_type);
END mapa;
/

-- crear cuerpo mapa
CREATE OR REPLACE PACKAGE BODY mapa AS
    PROCEDURE init_matrix(out_matrix OUT matrix_type) IS
        matrix matrix_type := matrix_type();
        fila row_type := row_type();
    BEGIN
        FOR i IN 1..10 LOOP
            matrix.EXTEND;
            FOR j IN 1..10 LOOP
                fila.EXTEND;
                fila(j) := '[ ]';
            END LOOP;
            matrix(i) := fila;
            fila := row_type();
        END LOOP;
        out_matrix := matrix;
    END init_matrix;
    
     -- Procedimiento para mostrar la matriz
    PROCEDURE display_matrix(matrix IN matrix_type) IS
    BEGIN
        -- Mostrar la matriz como un tablero
          DBMS_OUTPUT.PUT_LINE('-  0   1   2   3   4   5   6   7   8   9');
          FOR i IN 1..10 LOOP
            DBMS_OUTPUT.PUT( ' '||i-1 ||' ' );
            FOR j IN 1..10 LOOP
                -- Mostrar el valor de cada celda
                DBMS_OUTPUT.PUT(matrix(j)(i) || ' ');
            END LOOP;
            -- Nueva línea para separar las filas
            DBMS_OUTPUT.NEW_LINE;
        END LOOP;
    END display_matrix;
END mapa;
/


SET SERVEROUTPUT ON;
DECLARE
    -- Declarar una variable para contener la matriz del mapa
    v_mapa matrix_type;
BEGIN
    -- Inicializar la matriz del mapa
    mapa.init_matrix(v_mapa);
    
    -- Mostrar la matriz del mapa por consola
    mapa.display_matrix(v_mapa);
END;
/

--- CREAR BARCOS ---
----------------Se crea un objeto para las coordenadas-------------------
CREATE OR REPLACE TYPE Coordenada_Type AS OBJECT (
    X NUMBER,
    Y NUMBER
);
/
 ------------------Se crea una tabla anidada con las coordenadas(lista de las coordenadas de cada barco)----------------
CREATE OR REPLACE TYPE Coordenadas_Table_Type AS TABLE OF Coordenada_Type;
/

-------------------Se crea el objeto Barco------------------
CREATE OR REPLACE TYPE Barco_Type AS OBJECT (
    id_barco NUMBER,
    tipo CHAR(1),
    coordenadas Coordenadas_Table_Type,
    direccion VARCHAR2(1), --H o V
    vida NUMBER
);
/
---------------------Se crea el Arreglo de barcos-------------------------
CREATE OR REPLACE TYPE Barco_Varray_Type AS VARRAY(100) OF Barco_Type;
/

CREATE OR REPLACE TYPE Barcos_hundidos AS VARRAY(4) OF NUMBER;
/

----------------Funcion para verificar si una coordenada ya est  ocupada----------------------------
CREATE OR REPLACE FUNCTION coordenada_ocupada(coordenada Coordenada_Type, coordenadas_ocupadas Coordenadas_Table_Type) RETURN BOOLEAN IS
BEGIN
    FOR i IN 1..coordenadas_ocupadas.COUNT LOOP
        IF coordenadas_ocupadas(i).X = coordenada.X AND coordenadas_ocupadas(i).Y = coordenada.Y THEN
            RETURN TRUE;
        END IF;
    END LOOP;
    RETURN FALSE;
END coordenada_ocupada;
/

-----------------------------Función para verificar si un barco se superpone con otros barcos--------------------------
CREATE OR REPLACE FUNCTION se_superpone(coordenadas Coordenadas_Table_Type, coordenadas_ocupadas Coordenadas_Table_Type) RETURN BOOLEAN IS
BEGIN
    FOR i IN 1..coordenadas.COUNT LOOP
        IF coordenada_ocupada(coordenadas(i), coordenadas_ocupadas) THEN
            RETURN TRUE;
        END IF;
    END LOOP;
    RETURN FALSE;
END se_superpone;

/

CREATE SEQUENCE sq_id_barcos
START WITH 1
INCREMENT BY 1
NOCACHE;
/

CREATE OR REPLACE FUNCTION generar_barcos_generico(tipo_barco IN CHAR, tamano_barco IN NUMBER, num_barcos IN NUMBER, coordenadas_ocupadas_tipos IN Coordenadas_Table_Type) 
RETURN Barco_Varray_Type IS
    barcos Barco_Varray_Type := Barco_Varray_Type();
    coordenadas_ocupadas Coordenadas_Table_Type := Coordenadas_Table_Type();
    v_id NUMBER;
BEGIN
    FOR i IN 1..num_barcos LOOP
        -- Generar coordenada inicial aleatoria
        DECLARE
            coordenada_inicial Coordenada_Type := Coordenada_Type(ROUND(DBMS_RANDOM.VALUE(0, 10 - tamano_barco)), ROUND(DBMS_RANDOM.VALUE(0, 9)));
            direccion VARCHAR2(1) := CASE ROUND(DBMS_RANDOM.VALUE(1, 2)) WHEN 1 THEN 'H' ELSE 'V' END;
            coordenadas Coordenadas_Table_Type := Coordenadas_Table_Type();
            coordenada_disponible BOOLEAN := FALSE;
        BEGIN
            -- Bucle para intentar encontrar una coordenada disponible
            WHILE NOT coordenada_disponible LOOP
                -- Limpiar las coordenadas para cada intento
                coordenadas := Coordenadas_Table_Type();
                
                -- Generar coordenadas adicionales basadas en la dirección y tama o del barco
                FOR j IN 1..tamano_barco LOOP
                    IF direccion = 'H' THEN
                        IF coordenada_inicial.X + j - 1 <= 9 THEN
                            coordenadas.EXTEND;
                            coordenadas(coordenadas.LAST) := Coordenada_Type(coordenada_inicial.X + j - 1, coordenada_inicial.Y);
                        ELSE
                            EXIT;
                        END IF;
                    ELSE
                        IF coordenada_inicial.Y + j - 1 <= 9 THEN
                            coordenadas.EXTEND;
                            coordenadas(coordenadas.LAST) := Coordenada_Type(coordenada_inicial.X, coordenada_inicial.Y + j - 1);
                        ELSE
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;

                -- Verificar si las coordenadas son válidas y no se superponen
                IF coordenadas.COUNT = tamano_barco AND NOT se_superpone(coordenadas, coordenadas_ocupadas_tipos) AND NOT se_superpone(coordenadas, coordenadas_ocupadas) THEN
                    coordenada_disponible := TRUE; -- Establecer que se encontr  una coordenada disponible
                ELSE
                    -- Intentar con una nueva coordenada inicial
                    coordenada_inicial := Coordenada_Type(ROUND(DBMS_RANDOM.VALUE(0, 10 - tamano_barco)), ROUND(DBMS_RANDOM.VALUE(0, 9)));
                END IF;
            END LOOP;

            -- Si se encontró una coordenada disponible, crear el barco
            IF coordenada_disponible THEN
                barcos.EXTEND;
                SELECT sq_id_barcos.NEXTVAL INTO v_id FROM DUAL;
                barcos(barcos.LAST) := Barco_Type(v_id, tipo_barco, coordenadas, direccion, tamano_barco);
                FOR k IN 1..coordenadas.COUNT LOOP
                    coordenadas_ocupadas.EXTEND;
                    coordenadas_ocupadas(coordenadas_ocupadas.LAST) := coordenadas(k);
                END LOOP;
            END IF;
        END;
    END LOOP;
    RETURN barcos;
END;
/

CREATE OR REPLACE FUNCTION generar_barcos RETURN Barco_Varray_Type IS
    total_barcos Barco_Varray_Type := Barco_Varray_Type();
    coordenadas_ocupadas Coordenadas_Table_Type := Coordenadas_Table_Type();
BEGIN
    -- Generar acorazados
    FOR acorazado IN (SELECT * FROM TABLE(generar_barcos_generico('A', 4, 2, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(acorazado.id_barco, acorazado.tipo, acorazado.coordenadas, acorazado.direccion, acorazado.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL acorazado.coordenadas;
    END LOOP;
    
    -- Generar cruceros
    FOR crucero IN (SELECT * FROM TABLE(generar_barcos_generico('C', 3, 3, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(crucero.id_barco, crucero.tipo, crucero.coordenadas, crucero.direccion, crucero.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL crucero.coordenadas;
    END LOOP;

    -- Generar destructores
    FOR destructor IN (SELECT * FROM TABLE(generar_barcos_generico('D', 2, 2, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(destructor.id_barco, destructor.tipo, destructor.coordenadas, destructor.direccion, destructor.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL destructor.coordenadas;
    END LOOP;

    -- Generar submarinos
    FOR submarino IN (SELECT * FROM TABLE(generar_barcos_generico('S', 1, 1, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(submarino.id_barco, submarino.tipo, submarino.coordenadas, submarino.direccion, submarino.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL submarino.coordenadas;
    END LOOP;
    
    RETURN total_barcos;
END;

/

-------------------CREACIÓN DEL OBJETO JUGADA------------------
CREATE OR REPLACE TYPE jugada AS OBJECT (
    tipo CHAR(1),
    coordenadas Coordenadas_Table_Type
);
/
--------------Se crea EL VARRAY de jugadas------------------
CREATE OR REPLACE TYPE Jugadas_Varray_Type AS VARRAY(70) OF jugada;
/
CREATE TABLE tabla_juegos (
    id_juego NUMBER PRIMARY KEY,
    fecha_inicio DATE,
    barcos_usuario Barco_Varray_Type,
    jugadas Jugadas_Varray_Type,
    matrix_juego matrix_type,
    intentos NUMBER,
    hundidos Barcos_hundidos
);

-----------------Procedimiento para insertar un nuevo juego a la tabla-----------------------------
CREATE OR REPLACE PROCEDURE iniciar_juego (
    p_id_juego IN NUMBER,
    p_barcos_originales IN Barco_Varray_Type,
    matrix_juego IN matrix_type,
    p_intentos IN NUMBER
) AS
    barcos_h Barcos_hundidos;
BEGIN
    -- Insertar el juego en la tabla_juegos
    barcos_h := Barcos_hundidos();
    barcos_h.EXTEND;
    barcos_h(barcos_h.LAST) := 0;
    barcos_h.EXTEND;
    barcos_h(barcos_h.LAST) := 0;
    barcos_h.EXTEND;
    barcos_h(barcos_h.LAST) := 0;
    barcos_h.EXTEND;
    barcos_h(barcos_h.LAST) := 0;
    
    INSERT INTO tabla_juegos (id_juego, fecha_inicio,  barcos_usuario, jugadas ,matrix_juego, intentos, hundidos)
    VALUES (
        p_id_juego,
        SYSDATE,
        p_barcos_originales,
        Jugadas_Varray_Type(),
        matrix_juego,
        p_intentos,
        barcos_h
    );
END iniciar_juego;
/

----------------Procedimiento para actualizar la tabla de jugadas------------
CREATE OR REPLACE PROCEDURE actualizar_juego (
    p_id_juego IN NUMBER,
    p_id_barco_impactado IN NUMBER,
    p_jugada_tipo IN CHAR,
    p_jugada_coordenadas IN Coordenada_Type,
    tablero_usuario IN matrix_type
) AS
    v_jugadas Jugadas_Varray_Type;
    v_barcos Barco_Varray_Type;
    hundidos_aux Barcos_hundidos;
BEGIN
    -- Obtener las jugadas actuales
    SELECT jugadas INTO v_jugadas
    FROM tabla_juegos
    WHERE id_juego = p_id_juego;

    -- Obtener los barcos del usuario actuales
    SELECT barcos_usuario INTO v_barcos
    FROM tabla_juegos
    WHERE id_juego = p_id_juego;
    
    SELECT hundidos INTO hundidos_aux
    FROM tabla_juegos
    WHERE id_juego = p_id_juego;

    -- Verificar que el ID del barco no sea nulo
    IF p_id_barco_impactado IS NOT NULL THEN
        -- Actualizar la vida del barco impactado
        FOR i IN 1..v_barcos.COUNT LOOP
            IF v_barcos(i).id_barco = p_id_barco_impactado THEN
                v_barcos(i).vida := v_barcos(i).vida - 1;
                IF  v_barcos(i).vida = 0 THEN
                    DBMS_OUTPUT.PUT_LINE('Se hundio un barco de tipo: ' || v_barcos(i).tipo);
                    IF v_barcos(i).tipo =  'A' THEN
                        hundidos_aux(1) := hundidos_aux(1)+1;
                    ELSIF v_barcos(i).tipo =  'C' THEN
                        hundidos_aux(2) := hundidos_aux(2)+1;
                    ELSIF v_barcos(i).tipo =  'D' THEN
                        hundidos_aux(3) := hundidos_aux(3)+1;
                    ELSIF v_barcos(i).tipo =  'S' THEN
                        hundidos_aux(4) := hundidos_aux(4)+1;
                    END IF; 
                END IF;
                EXIT; -- Salir del bucle una vez que se actualice la vida del barco
            END IF;
        END LOOP;
    END IF;

    -- Agregar una nueva jugada al final del VARRAY
    v_jugadas.EXTEND;
    v_jugadas(v_jugadas.LAST) := jugada(p_jugada_tipo, Coordenadas_Table_Type(p_jugada_coordenadas));

    -- Actualizar la tabla con el nuevo VARRAY de jugadas y la vida del barco impactado
    UPDATE tabla_juegos
    SET jugadas = v_jugadas,
        barcos_usuario = v_barcos,
        matrix_juego = tablero_usuario,
        intentos = intentos - 1,
        hundidos = hundidos_aux
    WHERE id_juego = p_id_juego;
END actualizar_juego;
/

-------------------------Verificar tiro del usuario-------------------------
CREATE OR REPLACE FUNCTION verificar_intentos_restantes(
    juego_id IN NUMBER -- ID del juego en la tabla_juegos
) RETURN boolean IS
    intentos_restantes NUMBER;
BEGIN
    -- Obtener el número de intentos restantes del juego
    SELECT intentos INTO intentos_restantes
    FROM tabla_juegos
    WHERE id_juego = juego_id;

    -- Verificar si aún hay intentos disponibles
    IF intentos_restantes > 0 THEN
        RETURN TRUE;
    ELSE
        DBMS_OUTPUT.PUT_LINE('Se te han agotado los intentos! Perdiste!');
        pkt_juego.mostrar_barcos_generados(juego_id);
        RETURN FALSE;
    END IF;
END verificar_intentos_restantes;
/

CREATE OR REPLACE FUNCTION verificar_ganador(
    juego_id IN NUMBER -- ID del juego en la tabla_juegos
) RETURN BOOLEAN IS
    total_barcos INT;
    total_hundidos INT := 0;
BEGIN
    -- Obtener el número total de barcos del juego
    SELECT COUNT(*) INTO total_barcos
    FROM TABLE((SELECT barcos_usuario FROM tabla_juegos WHERE id_juego = juego_id));

    -- Obtener el número total de barcos hundidos de cada tipo
    SELECT SUM(num_barcos_hundidos) INTO total_hundidos
    FROM (
        SELECT SUM(column_value) AS num_barcos_hundidos
        FROM TABLE((SELECT hundidos FROM tabla_juegos WHERE id_juego = juego_id))
    );

    -- Verificar si todos los barcos han sido hundidos
    IF total_hundidos = total_barcos THEN
        DBMS_OUTPUT.PUT_LINE('Ganaste! Felicidades!');
        RETURN FALSE; -- Todos los barcos han sido hundidos, el jugador ha ganado
    ELSE
        RETURN TRUE; -- Al menos un barco aún no ha sido hundido, el juego continúa
    END IF;
END;
/

CREATE OR REPLACE FUNCTION verificar_tiro(
    juego_id IN NUMBER, -- ID del juego en la tabla_juegos
    x_coordinate IN NUMBER, -- Coordenada X del tiro del usuario
    y_coordinate IN NUMBER, -- Coordenada Y del tiro del usuario
    barcos Barco_Varray_Type, -- Matriz con los barcos ya generados
    tablero_usuario IN matrix_type -- Matriz del tablero del usuario
) RETURN matrix_type IS
    updated_tablero matrix_type := tablero_usuario; -- Matriz actualizada con el nuevo tiro
    barco_actual Barco_Type; -- Variable auxiliar para almacenar el barco actual
BEGIN
     IF verificar_intentos_restantes(juego_id) AND verificar_ganador(juego_id) THEN
        -- Verificar si la coordenada de tiro está dentro de los límites del tablero
        IF x_coordinate BETWEEN 0 AND 9 AND y_coordinate BETWEEN 0 AND 9 THEN
            -- Verificar si la coordenada de tiro no ha sido atacada previamente
            IF updated_tablero(x_coordinate + 1)(y_coordinate + 1) = '[ ]' THEN
                -- Verificar si el tiro golpea un barco
                FOR i IN 1..barcos.COUNT LOOP
                    FOR j IN 1..barcos(i).coordenadas.COUNT LOOP
                        IF barcos(i).coordenadas(j).X = x_coordinate AND barcos(i).coordenadas(j).Y = y_coordinate THEN
                            -- El tiro golpea un barco
                            -- Actualizar el estado del tablero del usuario para reflejar el impacto del tiro
                            updated_tablero(x_coordinate + 1)(y_coordinate + 1) := '[' || barcos(i).tipo || ']';
                            -- Almacenar el barco actual en la variable barco_actual
                            actualizar_juego(juego_id, barcos(i).id_barco, barcos(i).tipo, Coordenada_Type(x_coordinate, y_coordinate), updated_tablero);
                            -- Actualizar los datos del juego en la tabla_juegos
                        END IF;
                    END LOOP;
                    -- Marcar la coordenada como tiro realizado en el tablero del usuario
                    IF updated_tablero(x_coordinate + 1)(y_coordinate + 1) = '[ ]' THEN
                        updated_tablero(x_coordinate + 1)(y_coordinate + 1) := '[X]';
                        actualizar_juego(juego_id, null, 'X', Coordenada_Type(x_coordinate, y_coordinate), updated_tablero);
                    END IF;
                END LOOP;
                pkt_juego.calcular_tasa_cobertura(juego_id);
            ELSE
                DBMS_OUTPUT.PUT_LINE('Esta coordenada ya ha sido atacada anteriormente.');
            END IF;
        ELSE
            DBMS_OUTPUT.PUT_LINE('Coordenada de tiro fuera de los límites del tablero.');
        END IF;
    END IF;
    RETURN updated_tablero;
END;
/

--- CLASE JUEGO --- 
CREATE OR REPLACE PACKAGE pkt_juego AS 
  PROCEDURE crear_juego (id_juego NUMBER, dificultad IN NUMBER);
  PROCEDURE realizar_intento (id_juego NUMBER, coordenada_x IN NUMBER, coordenada_y IN NUMBER);
  PROCEDURE ver_mapa(id_juego IN NUMBER);
  PROCEDURE generar_comodin(id_juego IN NUMBER);
  PROCEDURE calcular_tasa_cobertura(id_juego IN NUMBER);
  PROCEDURE mostrar_barcos_generados(juego_id IN NUMBER);
END pkt_juego;
/

CREATE OR REPLACE  PACKAGE BODY pkt_juego AS 
    PROCEDURE crear_juego (id_juego NUMBER, dificultad IN NUMBER)
    AS
        v_barcos Barco_Varray_Type;
        -- Declarar una variable para contener la matriz del tablero del usuario despu s del primer tiro
        v_tablero_usuario matrix_type;
    BEGIN
        DBMS_OUTPUT.PUT_LINE('CREACIÓN DE JUEGO');
        v_barcos := generar_barcos();
        FOR i IN 1..v_barcos.COUNT LOOP
            DBMS_OUTPUT.PUT_LINE('BARCO CREADO' || v_barcos(i).vida);
        END LOOP;
        mapa.init_matrix(v_tablero_usuario);
        iniciar_juego(id_juego,v_barcos,v_tablero_usuario,dificultad);
        
    END crear_juego;
    
   PROCEDURE realizar_intento(id_juego IN NUMBER, coordenada_x IN NUMBER, coordenada_y IN NUMBER)
   AS
         v_barcos Barco_Varray_Type;
         tablero_usuario matrix_type;
    BEGIN
        DBMS_OUTPUT.PUT_LINE('REALIZANDO INTENTO');
        
        SELECT barcos_usuario INTO v_barcos
        FROM tabla_juegos
        WHERE id_juego = id_juego;
        
        SELECT matrix_juego INTO tablero_usuario
        FROM tabla_juegos
        WHERE id_juego = id_juego;
        
        tablero_usuario := verificar_tiro(id_juego, coordenada_x, coordenada_y, v_barcos , tablero_usuario);
    END realizar_intento;
    

    PROCEDURE ver_mapa(id_juego IN NUMBER)
    AS
        v_tablero_usuario matrix_type;
    BEGIN
        DBMS_OUTPUT.PUT_LINE('VISTA MAPITA');
        SELECT matrix_juego INTO v_tablero_usuario FROM tabla_juegos WHERE id_juego = id_juego;
        mapa.display_matrix(v_tablero_usuario);
    END ver_mapa;
    
    PROCEDURE generar_comodin(id_juego IN NUMBER)AS
    BEGIN
        DBMS_OUTPUT.PUT_LINE('---------COMODIN TURNOS EXTRA ASIGNADO---------');
        DBMS_OUTPUT.PUT_LINE('Obtuviste 5 turnos extra! Sigue jugando!');
        UPDATE tabla_juegos
        SET intentos = intentos + 5
        WHERE id_juego = id_juego;
    END generar_comodin;
    
    PROCEDURE calcular_tasa_cobertura(id_juego IN NUMBER) AS
        hundidos Barcos_hundidos;
        acorazados_total NUMBER;
        cruceros_total NUMBER;
        destructores_total NUMBER;
        submarinos_total NUMBER;
    BEGIN
        -- Obtener el varray de barcos hundidos
        SELECT hundidos INTO hundidos
        FROM tabla_juegos
        WHERE id_juego = id_juego;

        -- Contar el número total de cada tipo de barco en la tabla_juegos
        SELECT 
            SUM(CASE WHEN bu.tipo = 'A' THEN 1 ELSE 0 END) AS acorazados_total,
            SUM(CASE WHEN bu.tipo = 'C' THEN 1 ELSE 0 END) AS cruceros_total,
            SUM(CASE WHEN bu.tipo = 'D' THEN 1 ELSE 0 END) AS destructores_total,
            SUM(CASE WHEN bu.tipo = 'S' THEN 1 ELSE 0 END) AS submarinos_total
        INTO 
            acorazados_total, cruceros_total, destructores_total, submarinos_total
        FROM 
            tabla_juegos t,
            TABLE(t.barcos_usuario) bu
        WHERE 
            t.id_juego = id_juego;

        -- Mostrar la tasa de cobertura de los barcos hundidos para cada tipo de barco
        DBMS_OUTPUT.PUT_LINE('Tasa de Cobertura de Barcos Hundidos:');
        DBMS_OUTPUT.PUT_LINE('Acorazado: ' || hundidos(1) || '/' || acorazados_total);
        DBMS_OUTPUT.PUT_LINE('Cruceros: ' || hundidos(2) || '/' || cruceros_total);
        DBMS_OUTPUT.PUT_LINE('Destructores: ' || hundidos(3) || '/' || destructores_total);
        DBMS_OUTPUT.PUT_LINE('Submarinos: ' || hundidos(4) || '/' || submarinos_total);
    END calcular_tasa_cobertura;
    
        -- Procedimiento para mostrar los barcos generados en el mapa
    PROCEDURE mostrar_barcos_generados(juego_id IN NUMBER) AS
        v_barcos Barco_Varray_Type; -- Variable para almacenar los barcos generados
        v_mapa matrix_type; -- Matriz para mostrar los barcos
    BEGIN
        -- Obtener los barcos generados para el juego especificado
        SELECT barcos_usuario
        INTO v_barcos
        FROM tabla_juegos
        WHERE id_juego = juego_id;

        -- Inicializar el mapa con la función del paquete mapa
        mapa.init_matrix(v_mapa);

        -- Colocar los barcos en el mapa
        FOR i IN 1..v_barcos.COUNT LOOP
            FOR j IN 1..v_barcos(i).coordenadas.COUNT LOOP
                v_mapa(v_barcos(i).coordenadas(j).X + 1)(v_barcos(i).coordenadas(j).Y + 1) := '[' || v_barcos(i).tipo || ']';
            END LOOP;
        END LOOP;

        -- Mostrar el mapa con los barcos generados utilizando la función del paquete mapa
        mapa.display_matrix(v_mapa);
    END mostrar_barcos_generados;
END pkt_juego;
/

-------------------------Eliminar barcos-------------------
CREATE OR REPLACE PROCEDURE eliminar_barco (
    p_id_juego IN NUMBER,
    p_id_barco_eliminar IN NUMBER
) AS
    v_barcos Barco_Varray_Type;
    v_barcos_new Barco_Varray_type;
BEGIN
    -- Obtener los barcos del usuario actuales
    v_barcos_new := Barco_Varray_Type();
    SELECT barcos_usuario INTO v_barcos
    FROM tabla_juegos
    WHERE id_juego = p_id_juego;

    -- Verificar que el ID del barco no sea nulo

    FOR i IN 1..v_barcos.COUNT LOOP
        IF v_barcos(i).id_barco != p_id_barco_eliminar THEN
                v_barcos_new.EXTEND;
                v_barcos_new(v_barcos_new.LAST) := v_barcos(i);
        END IF;
    END LOOP;

    -- Actualizar la tabla con el nuevo VARRAY de jugadas y la vida del barco impactado
    UPDATE tabla_juegos
    SET barcos_usuario = v_barcos_new
    WHERE id_juego = p_id_juego;
END eliminar_barco;
/

-----------------------Mostrar barcos--------------------------
CREATE OR REPLACE PROCEDURE mostrar_barcos_generados(juego_id IN NUMBER)
IS
    v_barcos Barco_Varray_Type; -- Variable para almacenar los barcos generados
    v_tablero Matrix_Type; -- Matriz para mostrar los barcos
BEGIN
    -- Obtener los barcos generados para el juego especificado
    SELECT barcos_usuario
    INTO v_barcos
    FROM tabla_juegos
    WHERE id_juego = juego_id;

    -- Inicializar el tablero con espacios vacíos utilizando el método INIT_MATRIX
    mapa.init_matrix(v_tablero);

    -- Colocar los barcos en el tablero
    FOR i IN 1..v_barcos.COUNT LOOP
        FOR j IN 1..v_barcos(i).coordenadas.COUNT LOOP
            v_tablero(v_barcos(i).coordenadas(j).X + 1)(v_barcos(i).coordenadas(j).Y + 1) := '[' || v_barcos(i).tipo || ']';
        END LOOP;
    END LOOP;

    -- Mostrar el tablero con los barcos generados
    DBMS_OUTPUT.PUT_LINE('--- Barcos Generados ---');
    FOR i IN 1..10 LOOP
        FOR j IN 1..10 LOOP
            DBMS_OUTPUT.PUT(v_tablero(j)(i) || ' ');
        END LOOP;
        DBMS_OUTPUT.NEW_LINE;
    END LOOP;
END mostrar_barcos_generados;
/


------------------------------Añadir barcos-------------------------
CREATE OR REPLACE PROCEDURE añadir_barcos (
    p_id_juego IN NUMBER,
    c_a IN NUMBER, 
    c_c IN NUMBER,
    c_d IN NUMBER,
    c_s IN NUMBER
) AS
    total_barcos Barco_Varray_Type;
    coordenadas_ocupadas Coordenadas_Table_Type := Coordenadas_Table_Type();
BEGIN
    SELECT barcos_usuario INTO total_barcos
    FROM tabla_juegos
    WHERE id_juego = p_id_juego;
    
    FOR i IN 1..total_barcos.COUNT LOOP
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL total_barcos(i).coordenadas;
    END LOOP;
    
     FOR acorazado IN (SELECT * FROM TABLE(generar_barcos_generico('A', 4, c_a, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(acorazado.id_barco, acorazado.tipo, acorazado.coordenadas, acorazado.direccion, acorazado.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL acorazado.coordenadas;
    END LOOP;
    
    -- Generar cruceros
    FOR crucero IN (SELECT * FROM TABLE(generar_barcos_generico('C', 3, c_c, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(crucero.id_barco, crucero.tipo, crucero.coordenadas, crucero.direccion, crucero.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL crucero.coordenadas;
    END LOOP;

    -- Generar destructores
    FOR destructor IN (SELECT * FROM TABLE(generar_barcos_generico('D', 2, c_d, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(destructor.id_barco, destructor.tipo, destructor.coordenadas, destructor.direccion, destructor.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL destructor.coordenadas;
    END LOOP;

    -- Generar submarinos
    FOR submarino IN (SELECT * FROM TABLE(generar_barcos_generico('S', 1, c_s, coordenadas_ocupadas))) LOOP
        total_barcos.EXTEND;
        total_barcos(total_barcos.LAST) := Barco_Type(submarino.id_barco, submarino.tipo, submarino.coordenadas, submarino.direccion, submarino.vida);
        coordenadas_ocupadas := coordenadas_ocupadas MULTISET UNION ALL submarino.coordenadas;
    END LOOP;
    
    UPDATE tabla_juegos SET barcos_usuario = total_barcos WHERE id_juego = p_id_juego;
END añadir_barcos;
/

BEGIN 
    --mostrar_barcos_juego(1);
    --eliminar_barco(1, 17); -- id_juego y id_del barco a eliminar
    --mostrar_barcos_juego(1); 
    añadir_barcos(1,0,0,0,1);
END;

SET SERVEROUTPUT ON;
--- MAIN DEL PROGRAMA
DECLARE 
    opcion NUMBER:= 0; 
    dificultad NUMBER:=0;
    c_x NUMBER:= 0;
    c_y NUMBER:= 0;
BEGIN
    -- 1. Nueva partida
    
    -- SI TU ELECCIÓN FUE 1 INGRESE UN VALOR PARA DIFICULTAD
    -- 1. Facil (60 intentos)
    -- 2. Medio (50 intentos)
    -- 3. Dificil (40 intentos)
    dificultad := 1; 
    
    IF (dificultad=1) THEN
        pkt_juego.crear_juego(1, 60);
    ELSIF (dificultad=2)THEN
        pkt_juego.crear_juego(1, 50);
    ELSIF (dificultad=3)THEN
        pkt_juego.crear_juego(1, 40);
    ELSE
        DBMS_OUTPUT.PUT_LINE('OPCIÓN DE DIFICULTAD INCORRECTA');
    END IF;
END; 
/

DECLARE 
    c_x NUMBER:= 0;
    c_y NUMBER:= 0;
BEGIN
    c_x := 4;
    c_y := 3;
    pkt_juego.realizar_intento(1,c_x,c_y);
    pkt_juego.ver_mapa(1);
    --pkt_juego.generar_comodin(1);
    --pkt_juego.mostrar_barcos_generados(1);
END;


:- use_module(library(pce)).

% Predicado para mostrar una imagen en la interfaz
mostrar(V, D, M) :-
    new(I, image(V)),
    new(B, bitmap(I)),
    new(F2, figure),
    send(F2, display, B),
    new(D1, device),
    send(D1, display, F2),
    send(D, display, D1),
    send(D1, below(M)).

% Predicado para pedir al usuario que ingrese una planta o síntoma
pedir_input(Prompt, Label, Input) :-
    new(P, dialog),
    send(P, kind, transient),
    send(P, append, label(prompt)),
    send(P, append,
        new(TI, text_item(name, '',
                 message(P?ok_member, execute)))),
    send(P, append, button(ok, message(P, return, TI?selection))),
    send(P, append, button(cancel, message(P, return, @nil))),
    send(TI, label, Label),
    send(TI, selection, Prompt),
    get(P, confirm_centered, RawInput),
    send(P, show, @off),
    RawInput \== @nil,
    Input = RawInput.


% lista de Plantas.
iman('arnica','C:/Proyecto/yerberito/arnica.jpeg').
iman('borraja','C:/Proyecto/yerberito/borraja.jpeg').
iman('cempasuchil','C:/Proyecto/yerberito/cempasuchil.jpg').
iman('cempasuchil','C:/Proyecto/yerberito/dientedeleon.jpg').
iman('enebro','C:/Proyecto/yerberito/enebro.jpg').
iman('maguey','C:/Proyecto/yerberito/maguey.jpg').
iman('marihuana','C:/Proyecto/yerberito/marihuana.jpg').
iman('menta','C:/Proyecto/yerberito/menta.jpg').
iman('tripadejudas','C:/Proyecto/yerberito/tripasdejudas.jpg').

% Predicado para mostrar información sobre una planta medicinal
mostrar_planta_info(Planta) :-
    new(Dialog, dialog('Informacion sobre planta')),
    send(Dialog, size, size(650, 450)),
    send(Dialog, colour, colour(white)),
    send(Dialog, append, new(Menu, menu_bar)),
    send(Dialog, display, text('Planta medicinal:', left, normal), point(350, 20)),
    send(Dialog, display, text(Planta, left, bold), point(350, 40)),

    % Obtener y mostrar el nombre científico
    nombre_cientifico(Planta, NombreCientifico),
    send(Dialog, display, text('Nombre cientifico:', left, normal), point(350, 100)),
    send(Dialog, display, text(NombreCientifico, left, bold), point(350, 120)),

    send(Dialog, display, text('Propiedades curativas:', left, normal), point(350, 60)),
    findall(Propiedad, propiedad_curativa(Planta, Propiedad), Propiedades),
    atomic_list_concat(Propiedades, ', ', PropiedadesText),
    send(Dialog, display, text(PropiedadesText, left, bold), point(350, 80)),

    % Obtener y mostrar el consumo recomendado
    findall(Consumo, consumo(Planta, Consumo), Consumos),
    atomic_list_concat(Consumos, ', ', ConsumosText),
    send(Dialog, display, text('Formas de consumo:', left, normal), point(350, 140)),
    send(Dialog, display, text(ConsumosText, left, bold), point(350, 160)),

    % Obtener y mostrar la imagen de la planta
    iman(Planta, Foto),
    mostrar(Foto, Dialog, Menu),

    send(Dialog, open_centered).


% Predicado para mostrar información sobre un síntoma
mostrar_sintoma_info(Sintoma) :-
    new(Dialog, dialog('Informacion sobre sintoma')),
    send(Dialog, size, size(600, 500)),
    send(Dialog, append, label(nombre, 'Sintoma:')),
    send(Dialog, append, text(Sintoma)),
    send(Dialog, append, label(tratamiento, 'Tratamiento con plantas medicinales:')),
    findall(Planta, trata_enfermedad(Planta, Sintoma), Plantas),
    atomic_list_concat(Plantas, ', ', PlantasText),
    send(Dialog, append, text(PlantasText)),
    send(Dialog, open_centered).

% Predicado para mostrar información sobre una enfermedad
mostrar_enfermedad(enfermedad) :-
    new(Dialog, dialog('Informacion sobre enfermedad')),
    send(Dialog, size, size(650, 450)),
    send(Dialog, colour, colour(white)),
    send(Dialog, append, new(Text, text('Enfermedad:', left, normal))),
    send(Dialog, append, new(@lb, list_browser)),
    send(@lb, append, ''),
    findall(Planta, trata_enfermedad(Planta, enfermedad), Plantas),
    forall(member(Planta, Plantas),
           send(@lb, append, Planta)),
    send(Dialog, append, @lb),
    
    send(Dialog, open_centered).

% Predicado para iniciar la interfaz
start :-
    new(D, dialog('Conocimientos sobre plantas medicinales')),
    send(D, size, size(400, 600)),
    send(D, colour, colour(red)),
    send(D, append, new(Menu, menu_bar)),
    send(Menu, append, new(PlantaMenu, popup('Buscar planta'))),
    send(Menu, append, new(SintomaMenu, popup('Buscar sintoma'))),
    send(Menu, append, new(EnfermedadMenu, popup('Buscar enfermedad'))),
    % Opciones para buscar por planta
    send(PlantaMenu, append, menu_item(arnica, message(@prolog, mostrar_planta_info, arnica))),
    send(PlantaMenu, append, menu_item(borraja, message(@prolog, mostrar_planta_info, borraja))),
    send(PlantaMenu, append, menu_item(enebro, message(@prolog, mostrar_planta_info, enebro))),
    send(PlantaMenu, append, menu_item(diente_leon, message(@prolog, mostrar_planta_info, diente_leon))),
    send(PlantaMenu, append, menu_item(cempasuchil, message(@prolog, mostrar_planta_info, cempasuchil))),
    send(PlantaMenu, append, menu_item(maguey, message(@prolog, mostrar_planta_info, maguey))),
    send(PlantaMenu, append, menu_item(menta, message(@prolog, mostrar_planta_info, menta))),
    send(PlantaMenu, append, menu_item(marihuana, message(@prolog, mostrar_planta_info, marihuana))),
    send(PlantaMenu, append, menu_item(tripa_de_judas, message(@prolog, mostrar_planta_info, trip_de_judas))),
    
    % Opciones para buscar por síntoma
    send(SintomaMenu, append, menu_item(problemas_renales, message(@prolog, mostrar_sintoma_info, problemas_renales))),
    send(SintomaMenu, append, menu_item(fatiga_mental, message(@prolog, mostrar_sintoma_info, fatiga_mental))),
    send(SintomaMenu, append, menu_item(ansiedad_depresion, message(@prolog, mostrar_sintoma_info, ansiedad_depresion))),
    send(SintomaMenu, append, menu_item(problemas_respiratorios, message(@prolog, mostrar_sintoma_info, problemas_respiratorios))),
    send(SintomaMenu, append, menu_item(problemas_hepaticos, message(@prolog, mostrar_sintoma_info, problemas_hepaticos))),
    send(SintomaMenu, append, menu_item(dolores_menstruales, message(@prolog, mostrar_sintoma_info, dolores_menstruales))),
    send(SintomaMenu, append, menu_item(dolor_cronico, message(@prolog, mostrar_sintoma_info, dolor_cronico))),
    send(SintomaMenu, append, menu_item(dolores_musculares, message(@prolog, mostrar_sintoma_info, dolores_musculares))),
    send(SintomaMenu, append, menu_item(hematomas, message(@prolog, mostrar_sintoma_info, hematomas))),
    send(SintomaMenu, append, menu_item(inflamaciones, message(@prolog, mostrar_sintoma_info, inflamaciones))),
    send(SintomaMenu, append, menu_item(problemas_digestivos, message(@prolog, mostrar_sintoma_info, problemas_digestivos))),
    send(SintomaMenu, append, menu_item(quemaduras, message(@prolog, mostrar_sintoma_info, quemaduras))),
    send(SintomaMenu, append, menu_item(ulceras, message(@prolog, mostrar_sintoma_info, ulceras))),
    send(SintomaMenu, append, menu_item(heridas, message(@prolog, mostrar_sintoma_info, heridas))),
    send(SintomaMenu, append, menu_item(retencion_liquidos, message(@prolog, mostrar_sintoma_info, retencion_liquidos))),
    send(SintomaMenu, append, menu_item(tos, message(@prolog, mostrar_sintoma_info, tos))),
    send(SintomaMenu, append, menu_item(infecciones_cutaneas, message(@prolog, mostrar_sintoma_info, infecciones_cutaneas))),
    send(SintomaMenu, append, menu_item(perdida_apetito, message(@prolog, mostrar_sintoma_info, perdida_apetito))),

    % Opciones para buscar por enfermedad
    send(EnfermedadMenu, append, menu_item(viruela, message(@prolog, mostrar_enfermedad, viruela))),
    send(EnfermedadMenu, append, menu_item(sarampion, message(@prolog, mostrar_enfermedad, sarampion))),
    send(EnfermedadMenu, append, menu_item(varicela, message(@prolog, mostrar_enfermedad, varicela))),
    send(EnfermedadMenu, append, menu_item(escarlatina, message(@prolog, mostrar_enfermedad, escarlatina))),
    send(EnfermedadMenu, append, menu_item(gonorrea, message(@prolog, mostrar_enfermedad, gonorrea))),
    send(EnfermedadMenu, append, menu_item(reumatismos, message(@prolog, mostrar_enfermedad, reumatismos))),
    send(EnfermedadMenu, append, menu_item(tuberculosis, message(@prolog, mostrar_enfermedad, tuberculosis))),
    % Mostrar imagen de fondo y abrir la interfaz
    mostrar('C:/Proyecto/yerberito/portada.jpeg', D, Menu),
    send(D, open, point(0, 0)).


% Hechos sobre plantas medicinales y sus propiedades curativas
planta_medicinal(arnica).
propiedad_curativa(arnica, antiinflamatoria).
propiedad_curativa(arnica, analgesica).
propiedad_curativa(arnica, cicatrizante).
enfermedad(arnica,bronquitis).
enfermedad(arnica,neumonia).
consumo(arnica,te_con_raices).
nombre_cientifico(arnica,arnica_montana).

planta_medicinal(borraja).
propiedad_curativa(borraja, antiinflamatoria).
propiedad_curativa(borraja, diuretica).
propiedad_curativa(borraja, expectorante).
consumo(borraja,te_con_hojas_secas).
enfermedad(borraja,viruela).
enfermedad(borraja,sarampion).
enfermedad(borraja,varicela).
enfermedad(borraja,escarlatina).
nombre_cientifico(borraja,borrago_off).

planta_medicinal(cempasuchil).
propiedad_curativa(cempasuchil, antiséptica).
propiedad_curativa(cempasuchil, antiinflamatoria).
propiedad_curativa(cempasuchil, analgesica).
consumo(cempasuchil,te_con_hojas_secas),
nombre_cientifico(cempasuchil,tagetes_erecta).

planta_medicinal(diente_leon).
propiedad_curativa(diente_leon, depurativa).
propiedad_curativa(diente_leon, diuretica).
propiedad_curativa(diente_leon, hepatoprotectora).
consumo(diente_leon,te_con_hojas_secas).
nombre_cientifico(diente_leon,taraxacum_officinale).

planta_medicinal(enebro).
propiedad_curativa(enebro, diuretica).
propiedad_curativa(enebro, antiinflamatoria).
propiedad_curativa(enebro, antioxidante).
consumo(enebro,se_comen_sus_bayas).
nombre_cientifico(enebro,juniperus_communis).

planta_medicinal(maguey).
propiedad_curativa(maguey, cicatrizante).
propiedad_curativa(maguey, antiinflamatoria).
propiedad_curativa(maguey, digestiva).
consumo(maguey,te_con_la_penca).
enfermedad(maguey,tuberculosis).
enfermedad(maguey,gonorrea).
enfermedad(maguey,reumatismos).
nombre_cientifico(maguey,agabe_salmiana).

planta_medicinal(menta).
propiedad_curativa(menta, carminativa).
propiedad_curativa(menta, digestiva).
propiedad_curativa(menta, estimulante).
consumo(menta,masticar_las_hojas).
nombre_cientifico(menta,mentha_piperita).

planta_medicinal(marihuana).
propiedad_curativa(marihuana, analgesica).
propiedad_curativa(marihuana, antiinflamatoria).
propiedad_curativa(marihuana, estimulante_apetito).
propiedad_curativa(marihuana, ansiedad_depresion).
consumo(marihuana,te_con_hojas_secas).
consumo(marihuana,insienso).
nombre_cientifico(marihuana,cannabis_indica).

planta_medicinal(tripa_judas).
propiedad_curativa(tripa_judas, inmunoestimulante).
propiedad_curativa(tripa_judas, antioxidante).
propiedad_curativa(tripa_judas, anticoagulante).
consumo(tripa_judas,toma_extracto_de_la_hierba).
nombre_cientifico(tripa_judas,vitis_tilichea).

% Uso terapéutico de las plantas medicinales

uso_terapeutico(arnica, pomada).
uso_terapeutico(borraja, infusion).
uso_terapeutico(cempasuchil, infusion).
uso_terapeutico(diente_leon, te).
uso_terapeutico(enebro, infusion).
uso_terapeutico(maguey, gel).
uso_terapeutico(menta, infusion).
uso_terapeutico(marihuana, consumo).
uso_terapeutico(tripa_judas, cocina).

% Efectos secundarios y precauciones

efecto_secundario(arnica, ninguna).
efecto_secundario(borraja, ninguna).
efecto_secundario(cempasuchil, ninguna).
efecto_secundario(diente_leon, ninguna).
efecto_secundario(enebro, ninguna).
efecto_secundario(maguey, ninguna).
efecto_secundario(menta, ninguna).
efecto_secundario(marihuana, ninguna).
efecto_secundario(tripa_judas, ninguna).

precaucion(diente_leon, no_usar_con_calculos_biliares).
precaucion(maguey, no_ingerir).
precaucion(marihuana, uso_responsable).
precaucion(tripa_judas,no_la_hierva).

% Regla para determinar si una planta se puede usar para tratar una enfermedad
trata_enfermedad(Planta, Enfermedad) :-
    planta_medicinal(Planta),
    tiene_propiedad_curativa(Planta, Enfermedad).

% Predicado auxiliar para verificar si una planta tiene alguna propiedad curativa para una enfermedad dada
tiene_propiedad_curativa(Planta, Enfermedad) :-
    propiedad_curativa(Planta, Propiedad),
    sintoma_de(Propiedad, Enfermedad).

sintoma_de(antiinflamatoria, hematomas).
sintoma_de(antiinflamatoria, inflamaciones).
sintoma_de(antiinflamatoria, problemas_respiratorios).
sintoma_de(antiinflamatoria, problemas_digestivos).
sintoma_de(antiinflamatoria, problemas_hepaticos).
sintoma_de(antiinflamatoria, dolores_musculares).
sintoma_de(antiinflamatoria, artritis).
sintoma_de(analgesica, dolores_musculares).
sintoma_de(analgesica, dolores_menstruales).
sintoma_de(analgesica, dolor_cronico).
sintoma_de(cicatrizante, quemaduras).
sintoma_de(cicatrizante, ulceras).
sintoma_de(cicatrizante, heridas).
sintoma_de(diuretica, problemas_renales).
sintoma_de(diuretica, retencion_liquidos).
sintoma_de(expectorante, problemas_respiratorios).
sintoma_de(expectorante, tos).
sintoma_de(antiséptica, infecciones_cutaneas).
sintoma_de(hepatoprotectora, problemas_hepaticos).
sintoma_de(carminativa, problemas_digestivos).
sintoma_de(estimulante, fatiga_mental).
sintoma_de(estimulante_apetito, perdida_apetito).


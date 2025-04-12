def intro():
    print("\n🧠 Bienvenido a SamsuBrain 3000 🧠")
    print("El sistema experto que diagnostica tu TV Samsung sin necesidad de un ingeniero norcoreano.")
    print("Responde con 's' para sí, 'n' para no. ¡Vamos allá!\n")

def pregunta(mensaje):
    respuesta = input(mensaje + " (s/n): ").strip().lower()
    return respuesta == 's'

def diagnostico():
    reglas_activadas = []

    if pregunta("¿El televisor no enciende nada en absoluto?"):
        reglas_activadas.append("Verifica el cable de alimentación o el fusible interno.")

    if pregunta("¿La luz de standby está encendida pero la pantalla está negra?"):
        reglas_activadas.append("Puede ser un problema con el backlight o la tarjeta T-CON.")

    if pregunta("¿Se escucha sonido pero no hay imagen?"):
        reglas_activadas.append("La pantalla o el circuito de imagen puede estar dañado.")

    if pregunta("¿La imagen aparece y desaparece intermitentemente?"):
        reglas_activadas.append("Puede haber una mala conexión interna o capacitores defectuosos.")

    if pregunta("¿Hay líneas verticales u horizontales en la pantalla?"):
        reglas_activadas.append("Problema con el panel o el cable LVDS suelto.")

    if pregunta("¿El control remoto no responde?"):
        if pregunta("¿Probaste con pilas nuevas?"):
            reglas_activadas.append("El sensor IR del televisor puede estar fallando.")
        else:
            reglas_activadas.append("¡Ponle pilas nuevas al control, perezoso!")

    if pregunta("¿El televisor se apaga solo después de unos segundos?"):
        reglas_activadas.append("Podría estar activado el temporizador o ser una falla de la fuente de poder.")

    if pregunta("¿Se escucha un pitido o sonido extraño al encenderlo?"):
        reglas_activadas.append("Podría haber un corto o un componente electrónico quemado.")

    if pregunta("¿La imagen está distorsionada o los colores se ven raros?"):
        reglas_activadas.append("Podría ser el modo de imagen o problemas en la placa de video.")

    if pregunta("¿No puedes conectarte a internet desde el TV?"):
        if pregunta("¿Otras redes funcionan en casa?"):
            reglas_activadas.append("Revisa el adaptador de red del TV o restablece configuración de red.")
        else:
            reglas_activadas.append("Tu WiFi es el culpable. Reinicia el router.")

    if pregunta("¿El puerto HDMI no detecta ningún dispositivo?"):
        reglas_activadas.append("Podría estar dañado el puerto o es necesario cambiar la fuente de entrada.")

    if pregunta("¿La pantalla se queda en el logo de Samsung y no avanza?"):
        reglas_activadas.append("Fallo en el software. Reinstalar firmware o servicio técnico.")

    if pregunta("¿Se congela cuando usas una app (como Netflix, YouTube, etc)?"):
        reglas_activadas.append("Actualiza el software o reinstala la app.")

    if pregunta("¿El TV hace un ciclo infinito de reinicio?"):
        reglas_activadas.append("Fallo grave en software. Requiere reseteo de fábrica o reprogramación.")

    if not reglas_activadas:
        print("\n🤔 No se detectaron problemas conocidos. ¡Intenta apagar y volver a encender (funciona más de lo que crees)!")
    else:
        print("\n🔍 Diagnóstico:")
        for idx, r in enumerate(reglas_activadas, 1):
            print(f"  {idx}. {r}")
        print("\n💡 ¡Esperamos que esto te haya ayudado a resolver el misterio del Samsung rebelde!")

if __name__ == "__main__":
    intro()
    diagnostico()

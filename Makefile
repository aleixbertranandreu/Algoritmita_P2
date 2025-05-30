# Configuració de rangs de test
INICI=5
FI=10

# Nom base dels fitxers
practica=batalla

# Compilació automàtica del codi Haskell
$(practica): $(practica).hs
	ghc $< -o $(practica)

# Generar instàncies de prova
instances:
	python3 Generator.py $(INICI) $(FI)

# Test de la versió en Python
test-py: $(practica).py
	python3 Checker.py $(practica).py $(INICI) $(FI)

# Test de la versió en Haskell
test-hs: $(practica)  # compila si cal
	python3 Checker.py ./$(practica) $(INICI) $(FI)

# Crear zip per entregar la pràctica
zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile Checker.py $(practica).py $(practica).hs Generator.py

# Netejar fitxers intermedis
clean:
	$(RM) $(practica) $(practica).o $(practica).hi scripts.zip


INICI=5
FI=11

# Arxius específics
iter=batalla_iter.py
rec=batalla_rec.py
hs=batalla.hs
practica=batalla

# Compila la versió Haskell
$(practica): $(hs)
	ghc $<

# Genera instàncies d'entrada
instances: 
	python3 Generator.py $(INICI) $(FI)

# Test versió iterativa
test_iter: $(iter)
	python3 Checker.py $(iter) $(INICI) $(FI)

# Test versió recursiva
test_rec: $(rec)
	python3 Checker.py $(rec) $(INICI) $(FI)

# Crear zip amb tots els scripts
zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile Checker.py Generator.py $(iter) $(rec) $(hs) README.md

# Neteja fitxers de compilació Haskell
clean:
	$(RM) $(practica) $(practica).o $(practica).hi

# Entrega final completa
entrega:
	zip PR2_Algoritmia_Bertran-Pallàs.zip $(iter) $(rec) Checker.py Generator.py Makefile README.md

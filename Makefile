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

# Test versió Haskell (opcional)
# test_hs: $(practica)
#	python3 Checker.py ./$(practica) $(INICI) $(FI)

# Crear zip amb tot el necessari
zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile Checker.py $(iter) $(rec) $(hs) Generator.py

# Neteja fitxers GHC
clean:
	$(RM) $(practica) $(practica).o $(practica).hi

# Entrega final
entrega:
	zip PR2_Algoritmia_Bertran-Pallàs.zip $(iter) $(rec) $

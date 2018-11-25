Usare: Mettere la formula:

  (add-to-list 'auto-mode-alist
               '("^/home/ale/etc/ricette/" . ricette-mode))

in ~/.emacs e garantire che questo documento (ricette-mode.el)
sia in una "directory" che è presente in `load-path'.  (Variare come vuoi.)

La funzione `ricetta-nuova' domanda il nome della ricetta e prepara
il buffer vuoto con lo stile desiderato (titoli ecc.).
È possibile invocare questa funzione automaticamente aggiungendo la formula:

  (add-hook 'ricette-mode-hook 'ricetta-nuova)

Questo programma è dedicato alla mia maestra -- Alessandra Bertoni
(e il suo cervello bello 8-).

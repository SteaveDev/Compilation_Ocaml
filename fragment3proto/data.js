Essayer {
    x+2;
    Ecrire (bar(i*3));

} Rattraper (err) { Ecrire (err);}

Essayer {
    x+2;
    y="salut";
    Ecrire (bar(i*3));
} Rattraper (err) { Ecrire (err);}

Pour (Var i = 10; i > 0; i=i-1) {
    Essayer {
        Ecrire (bar(i*3));
    } Rattraper (err) { Ecrire (err);}
}
Fonction bar (i) {
    Si (i === 0) Lancer (42); Sinon x+2;
    Retourner (i);
}



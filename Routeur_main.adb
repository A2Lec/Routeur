with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Routeur;               

procedure Routeur_Main is

    Nom_Fichier_Table     : Unbounded_String;
    Nom_Fichier_Paquets   : Unbounded_String;
    Nom_Fichier_Resultats : Unbounded_String;


    La_Table_Routage : Routeur.T_Table_Routage;

begin

    Routeur.Recuperer_Arguments (
        Table     => Nom_Fichier_Table,
        Paquets   => Nom_Fichier_Paquets,
        Resultats => Nom_Fichier_Resultats
    );

    Put_Line("Utilisation de la table : " & To_String(Nom_Fichier_Table));
    Put_Line("Lecture des paquets   : " & To_String(Nom_Fichier_Paquets));
    Put_Line("Ecriture resultats    : " & To_String(Nom_Fichier_Resultats));

    Routeur.Construire_Table (
        Nom_Fichier => Nom_Fichier_Table,
        La_Table    => La_Table_Routage
    );


    Routeur.Traiter_Fichiers_Paquets (
        Fichier_Paquets   => Nom_Fichier_Paquets,
        Fichier_Resultats => Nom_Fichier_Resultats,
        La_Table          => La_Table_Routage
    );


    Put_Line("Traitement termine.");

end Routeur_Main;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Routeur;   use Routeur;  

procedure Routeur_Main is

    Nom_Fichier_Table     : Unbounded_String;
    Nom_Fichier_Paquets   : Unbounded_String;
    Nom_Fichier_Resultats : Unbounded_String;

    La_Table_Routage : Tab_Routage.T_LCA;

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
        Table_Routage => La_Table_Routage,
        Table    => Nom_Fichier_Table
    );


    Routeur.Traiter_Fichiers_Paquets (
        Paquets => Nom_Fichier_Paquets,
        Resultats => Nom_Fichier_Resultats,
        Table_Routage => La_Table_Routage
    );

    Tab_Routage.Detruire (La_Table_Routage);

end Routeur_Main;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Routeur_Cache_Liste;   use Routeur_Cache_Liste;
with Cache_Liste;  use Cache_Liste;

procedure Routeur_LL is


    Nom_Fichier_Table     : Unbounded_String;
    Nom_Fichier_Paquets   : Unbounded_String;
    Nom_Fichier_Resultats : Unbounded_String;
    Taille                : Integer;
    Politique_Str         : Unbounded_String;
    Statistiques          : Boolean;
    Politique             : T_Politique;

    La_Table_Routage : Tab_Routage.T_LCA;

begin

    Recuperer_Arguments (
        Table        => Nom_Fichier_Table,
        Paquets      => Nom_Fichier_Paquets,
        Resultats    => Nom_Fichier_Resultats,
        Taille       => Taille,
        Politique    => Politique_Str,
        Statistiques => Statistiques
    );


    Put_Line("Utilisation de la table    : " & To_String(Nom_Fichier_Table));
    Put_Line("Lecture des paquets        : " & To_String(Nom_Fichier_Paquets));
    Put_Line("Ecriture resultats         : " & To_String(Nom_Fichier_Resultats));
    Put_Line("Taille du cache            : " & Integer'Image(Taille));
    Put_Line("Politique du cache         : " & To_String(Politique_Str));
    if Statistiques then
        Put_Line ("Statistiques activées");
    else
        Put_Line ("Statistiques désactivées");
    end if;
    New_Line;

    Construire_Table (
        Table_Routage => La_Table_Routage,
        Table         => Nom_Fichier_Table
        );

    if To_String(Politique_Str) = "FIFO" then
        Politique := FIFO;
    elsif To_String(Politique_Str) = "LRU" then
        Politique := LRU;
    else
        Politique := LFU;
    end if;

    Traiter_Fichiers_Paquets (
        Paquets       => Nom_Fichier_Paquets,
        Resultats     => Nom_Fichier_Resultats,
        Table_Routage => La_Table_Routage,
        Taille        => Taille,
        Politique     => Politique
    );

end Routeur_LL;


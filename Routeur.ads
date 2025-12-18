with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA; 

package Routeur is

    package Tab_Routage is new LCA (T_Cle => Unbounded_String, T_Valeur => Unbounded_String);
    subtype T_Table_Routage is Tab_Routage.T_LCA;

    procedure Recuperer_Arguments (
        Table     : out Unbounded_String;
        Paquets   : out Unbounded_String;
        Resultats : out Unbounded_String
    );

    procedure Construire_Table (
        Nom_Fichier : in Unbounded_String;
        La_Table    : out T_Table_Routage
    );


    procedure Traiter_Fichiers_Paquets (
        Fichier_Paquets   : in Unbounded_String;
        Fichier_Resultats : in Unbounded_String;
        La_Table          : in T_Table_Routage
    );

end Routeur;
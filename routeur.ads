with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA;
with Adresse_IP;  use Adresse_IP;

package Routeur is

    type T_Route is record
       Masque : T_IP;
       Inter  : Unbounded_String;
    end record;

    package Tab_Routage is new LCA (T_Cle => T_IP, T_Valeur => T_Route);
    use Tab_Routage;

    
    procedure Recuperer_Arguments (
        Table     : out Unbounded_String;
        Paquets   : out Unbounded_String;
        Resultats : out Unbounded_String
    );

    procedure Construire_Table (
	Table_Routage    : out T_LCA;
        Table : in Unbounded_String
        
    );


    procedure Traiter_Fichiers_Paquets (
        Paquets   : in Unbounded_String;
        Resultats : in Unbounded_String;
        Table_Routage : in T_LCA
    );
    
end Routeur;

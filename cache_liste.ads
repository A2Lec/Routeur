with LCA;
with Adresse_IP;  use Adresse_IP;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
    Capacite : in Integer;

package Cache_Liste is

    type T_Politique is (FIFO, LRU, LFU);

    type T_Destination is record
        Adresse : T_IP;
        Occurrences : Integer;
    end record;

    type T_Tab_Adresses is Array (1..Capacite) of T_Destination;

    type T_Ordre_Politique is record
        Adresses : T_Tab_Adresses;
        Taille : Integer;
    end record;

    type T_Statistiques is record
        Nombre_Routes : Float;
        Nombre_Defauts : Float;
    end record;

    type T_Route is record
       Masque : T_IP;
       Inter  : Unbounded_String;
    end record;

    package Contenu is new LCA (T_Cle => T_IP, T_Valeur => T_Route);
    use Contenu;

    type T_Cache_Liste is record
        Capacite : Integer;
        Contenu : T_LCA;
        Politique : T_Politique;
        Ordre_Politique : T_Ordre_Politique;
        Statstiques : T_Statistiques;

    end record;

    procedure Creer (Cache : out T_Cache_Liste; Politique : in T_Politique);

    procedure Supprimer (Cache : in out T_Cache_Liste);

    procedure Detruire (Cache : in out T_Cache_Liste);

    procedure Ajouter (Cache : in out T_Cache_Liste; Adresse : in T_IP; Interface_S : Unbounded_String; Bonne_Interface_S : Unbounded_String);

end Cache_Liste;
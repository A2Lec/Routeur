with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package IP is

    type T_IP is new Unbounded_String;

    -- revient à supprimer le premier 1 en partant de la droite
    -- dans la décomposition de Entier en binaire 
    function Modifie_Entier (Entier : Integer) return Integer;
   
    -- renvoie le Kième entier (K entre 1 et 4) de l'adresse et fournit l'indice de son premier caractère
    procedure Kieme_Adresse (K : in Integer; Adresse : in T_IP; Indice : out Integer; Valeur : out Integer);

    -- renvoie l'indice et la valeur de la premiere valeur non nulle en partant de la droite
    procedure Adresse_Non_Nul (Adresse : in T_IP; Indice : out Integer; Valeur : out Integer);

    -- revient à supprimer le premier 1 en partant de la droite
    -- dans la décomposition de Adresse en binaire
    procedure Masquer_Bit (Adresse : in out T_IP);

    -- renvoie le nombre de 0 à droite avant le premier 1 
    -- dans l'écriture de Valeur en binaire
    function Zero_Entier (Valeur : Integer) return Integer;
 
    -- renvoie le nombre de 0 à droite avant le premier 1 
    -- dans l'écriture de Adresse en binaire
    function Adresse_Zero_Bit (Adresse : T_IP) return Integer;

    -- masque Adresse si besoin par rapport au masque qui lui est associé
    procedure Masquer_Adresse (Adresse : in out T_IP; Masque : in T_IP);

    function Ub_To_Ip (Chaine : Unbounded_String) return T_IP;

    function Ip_To_Ub (Adresse : T_IP) return Unbounded_String;

end IP;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Adresse_IP;                use Adresse_IP;

-- Trie pour le cache
package Trie is
	
	subtype T_Longueur_Prefixe is Integer range 0..32;
	-- type T_Profondeur is range 0..32;
	
	type T_Trie is limited private;

	procedure Initialiser(trie : out T_Trie);
	
	function EstVide(trie : in T_Trie) return Boolean;
	
	function EstFeuille(trie : in T_Trie) return Boolean;
	
	function Taille(trie : in T_Trie) return Integer;

	procedure Ajouter(trie : in out T_Trie; 
	                  adresse : in T_IP;
	                  longueur_prefixe : in T_Longueur_Prefixe;
	                  interface_sortie : in Unbounded_String);

	-- plus long prefixe
	function Rechercher(trie : in T_Trie; 
	                    adresse : in T_IP;
	                    interface_sortie : out Unbounded_String) return Boolean;

	procedure Supprimer(trie : in out T_Trie; 
	                    adresse : in T_IP;
	                    longueur_prefixe : in T_Longueur_Prefixe);
	
	procedure Vider(trie : in out T_Trie);
	
	procedure Afficher(trie : in T_Trie);

private
	type T_Bit is range 0..1;
	
	type T_Enfants is array (T_Bit) of T_Trie;

	type T_Cellule_Trie is record
		Enfants : T_Enfants;
		Est_Route : Boolean;
		Interface_Sortie : Unbounded_String;
	end record;	

	type T_Trie is access T_Cellule_Trie;	

end Trie;

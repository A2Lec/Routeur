with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Cache_Trie is

	procedure Initialiser(
		Cache : out T_Cache;
		Taille_Max : in Integer;
		Politique : in T_Politique) is
		-- i : Integer;
	begin
		Initialiser(Cache.Trie_Routes);
		Cache.Taille_Actuelle := 0;
		Cache.Taille_Maximale := Taille_Max;
		Cache.Politique_Cache := Politique;
		Cache.Nb_Demandes_Total := 0;
		Cache.Nb_Defauts_Cache := 0;
		Cache.Horloge := 0;
	end Initialiser;

	function Est_Vide(Cache : in T_Cache) return Boolean is
	begin
		return Cache.Taille_Actuelle = 0;
	end Est_Vide;

	function Est_Plein(Cache : in T_Cache) return Boolean is
	begin
		if Cache.Taille_Maximale > 0 then
			return Cache.Taille_Actuelle >= Cache.Taille_Maximale;
		else
			return True;
		end if;
	end Est_Plein;

	function Taille(Cache : in T_Cache) return Integer is
	begin
		return Cache.Taille_Actuelle;
	end Taille;

	function Taille_Max(Cache : in T_Cache) return Integer is
	begin
		return Cache.Taille_Maximale;
	end Taille_Max;

	function Rechercher(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Interface_Sortie : out Unbounded_String) return Boolean is
		Trouve : Boolean;
		-- idx : Integer := 0;
	begin
		-- Put_Line("Recherche dans cache");
		Cache.Nb_Demandes_Total := Cache.Nb_Demandes_Total + 1;
		
		Trouve := Rechercher(Cache.Trie_Routes, Adresse, Interface_Sortie);
		
		if not Trouve then
			Cache.Nb_Defauts_Cache := Cache.Nb_Defauts_Cache + 1;
		else
			-- maj LRU
			if Cache.Politique_Cache = LRU then
				for I in 1..Cache.Taille_Actuelle loop
					if To_String(Cache.Routes(I).Adresse) = To_String(Adresse) or
					   To_String(Cache.Routes(I).Interface_Sortie) = To_String(Interface_Sortie) then
						Cache.Horloge := Cache.Horloge + 1;
						Cache.Routes(I).Horodatage := Cache.Horloge;
						exit;
					end if;
				end loop;
			end if;
			
			-- maj LFU
			if Cache.Politique_Cache = LFU then
				for I in 1..Cache.Taille_Actuelle loop
					if To_String(Cache.Routes(I).Adresse) = To_String(Adresse) or
					   To_String(Cache.Routes(I).Interface_Sortie) = To_String(Interface_Sortie) then
						Cache.Routes(I).Compteur := Cache.Routes(I).Compteur + 1;
						exit;
					end if;
				end loop;
			end if;
		end if;
		
		return Trouve;
	end Rechercher;

	--------------
	procedure Ajouter(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Longueur_Prefixe : in Integer;
		Interface_Sortie : in Unbounded_String) is
		
		function Trouver_Victime return Integer is
			iv : Integer := 1;
			mh : Integer := Cache.Routes(1).Horodatage;
			mc : Integer := Cache.Routes(1).Compteur;
			-- min_idx : Integer;
		begin
			case Cache.Politique_Cache is
				when FIFO =>
					for I in 2..Cache.Taille_Actuelle loop
						if Cache.Routes(I).Horodatage < mh then
							mh := Cache.Routes(I).Horodatage;
							iv := I;
						end if;
					end loop;
					
				when LRU =>
					for I in 2..Cache.Taille_Actuelle loop
						if Cache.Routes(I).Horodatage < mh then
							mh := Cache.Routes(I).Horodatage;
							iv := I;
						end if;
					end loop;
					
				when LFU =>
					for I in 2..Cache.Taille_Actuelle loop
						if Cache.Routes(I).Compteur < mc then
							mc := Cache.Routes(I).Compteur;
							iv := I;
						end if;
					end loop;
			end case;
			return iv;
		end Trouver_Victime;
		
		iv : Integer;
	begin
		if Est_Plein(Cache) then
			iv := Trouver_Victime;
			Supprimer(Cache.Trie_Routes, Cache.Routes(iv).Adresse, Cache.Routes(iv).Longueur_Prefixe);
			for I in iv..Cache.Taille_Actuelle - 1 loop
				Cache.Routes(I) := Cache.Routes(I + 1);
			end loop;
			Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
		end if;
		
		Ajouter(Cache.Trie_Routes, Adresse, Longueur_Prefixe, Interface_Sortie);
		
		Cache.Taille_Actuelle := Cache.Taille_Actuelle + 1;
		Cache.Horloge := Cache.Horloge + 1;
		Cache.Routes(Cache.Taille_Actuelle).Adresse := Adresse;
		Cache.Routes(Cache.Taille_Actuelle).Longueur_Prefixe := Longueur_Prefixe;
		Cache.Routes(Cache.Taille_Actuelle).Interface_Sortie := Interface_Sortie;
		Cache.Routes(Cache.Taille_Actuelle).Horodatage := Cache.Horloge;
		Cache.Routes(Cache.Taille_Actuelle).Compteur := 0;
	end Ajouter;

	procedure Supprimer(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Longueur_Prefixe : in Integer) is
		Trouve : Boolean := False;
		Indice : Integer;
	begin
		for I in 1..Cache.Taille_Actuelle loop
			if To_String(Cache.Routes(I).Adresse) = To_String(Adresse) and
			   Cache.Routes(I).Longueur_Prefixe = Longueur_Prefixe then
				Trouve := True;
				Indice := I;
				exit;
			end if;
		end loop;
		
		if Trouve then
			Supprimer(Cache.Trie_Routes, Adresse, Longueur_Prefixe);
			for I in Indice..Cache.Taille_Actuelle - 1 loop
				Cache.Routes(I) := Cache.Routes(I + 1);
			end loop;
			Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
		end if;
	end Supprimer;

	procedure Vider(Cache : in out T_Cache) is
	begin
		Vider(Cache.Trie_Routes);
		Initialiser(Cache.Trie_Routes);
		Cache.Taille_Actuelle := 0;
	end Vider;

	procedure Afficher(Cache : in T_Cache) is
	begin
		if Est_Vide(Cache) then
			Put_Line("Cache vide");
		else
			Put("Cache (");
			Put(Cache.Taille_Actuelle, 0);
			Put("/");
			Put(Cache.Taille_Maximale, 0);
			Put(" routes, politique: ");
			case Cache.Politique_Cache is
				when FIFO => Put("FIFO");
				when LRU => Put("LRU");
				when LFU => Put("LFU");
			end case;
			Put_Line(")");
			
			for I in 1..Cache.Taille_Actuelle loop
				Put(To_String(Cache.Routes(I).Adresse));
				Put(" /");
				Put(Cache.Routes(I).Longueur_Prefixe, 0);
				Put(" -> ");
				Put(To_String(Cache.Routes(I).Interface_Sortie));
				
				if Cache.Politique_Cache = LFU then
					Put(" (freq: ");
					Put(Cache.Routes(I).Compteur, 0);
					Put(")");
				end if;
				
				New_Line;
			end loop;
		end if;
	end Afficher;

	procedure Afficher_Stats(Cache : in T_Cache) is
		Taux : Float;
	begin
		Put_Line("=== Statistiques du cache ===");
		Put("Politique         : ");
		case Cache.Politique_Cache is
			when FIFO => Put_Line("FIFO");
			when LRU => Put_Line("LRU");
			when LFU => Put_Line("LFU");
		end case;
		
		Put("Taille            : ");
		Put(Cache.Taille_Actuelle, 0);
		Put("/");
		Put(Cache.Taille_Maximale, 0);
		New_Line;
		
		Put("Nb demandes       : ");
		Put(Cache.Nb_Demandes_Total, 0);
		New_Line;
		
		Put("Nb defauts cache  : ");
		Put(Cache.Nb_Defauts_Cache, 0);
		New_Line;
		
		if Cache.Nb_Demandes_Total > 0 then
			Taux := (Float(Cache.Nb_Defauts_Cache) / Float(Cache.Nb_Demandes_Total)) * 100.0;
			Put("Taux de defaut    : ");
			Put(Taux, Fore => 1, Aft => 2, Exp => 0);
			Put_Line(" %");
		end if;
		
		Put_Line("============================");
	end Afficher_Stats;

	function Nb_Demandes(Cache : in T_Cache) return Integer is
	begin
		return Cache.Nb_Demandes_Total;
	end Nb_Demandes;

	function Nb_Defauts(Cache : in T_Cache) return Integer is
	begin
		return Cache.Nb_Defauts_Cache;
	end Nb_Defauts;

	function Taux_Defaut(Cache : in T_Cache) return Float is
	begin
		if Cache.Nb_Demandes_Total = 0 then
			return 0.0;
		else
			return (Float(Cache.Nb_Defauts_Cache) / Float(Cache.Nb_Demandes_Total)) * 100.0;
		end if;
	end Taux_Defaut;

	procedure Reinitialiser_Stats(Cache : in out T_Cache) is
	begin
		Cache.Nb_Demandes_Total := 0;
		Cache.Nb_Defauts_Cache := 0;
	end Reinitialiser_Stats;

end Cache_Trie;

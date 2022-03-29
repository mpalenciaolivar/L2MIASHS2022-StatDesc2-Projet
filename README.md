L2MIASHS2022-StatDesc2-Projet
==============================

Ce dépôt constitue plusieurs choses à la fois ; il contient :
- les données à utiliser ;
- les consignes ;
- des ressources additionnelles ;
- un template de base *facultatif* pour le rendu du projet de Statistique descriptive 2.

Organisation du projet
------------

    ├── LICENSE
    ├── README.md          <- Le document que vous consultez actuellement.
    ├── data
    │   ├── external       <- Données provenant de sources tierces.
    │   ├── interim        <- Données transformées, dans un état intermédiaire.
    │   ├── processed      <- Données à utiliser pour la modélisation.
    │   └── raw            <- Données originelles.
    │
    ├── docs               <- Documents liés au projet.
    │
    ├── models             <- Modèles entraînés, sérialisés, prédictions, résumés
    │
    ├── notebooks          <- R notebooks. La convention d'appellation est un numéro (pour l'ordre),
    │                         les initiales du créateur, et une courte description délimitée `-`, par exemple
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Dictionnaires de données, manuels et tout autre matériel explicatif.
    │
    ├── reports            <- Analyses générées sous formes de documents HTML, PDF, LaTeX, etc.
    │   └── figures        <- Graphiques générés et figures à utiliser dans le rapport.
    │
    ├── requirements.R   <- Les packages/bibliothèques/librairies nécessaires au bon fonctionnement du projet
    │
    └── src                <- Code source.
        │
        ├── data           <- Scripts à utiliser pour télécharger/générer les données
        │   └── make_dataset.R  <- Ex de script (pas inclus)
        │
        ├── features       <- Scripts pour passer de la donnée brute aux features
        │   └── build_features.R  <- Ex de script (pas inclus)
        │
        ├── models         <- Scripts pour entraîner les modèles et faire des prédictions
        │   ├── predict.R  <- Ex de script (pas inclus)
        │   └── train_model.R  <- Ex de script (pas inclus)
        │
        └── visualization  <- Scripts pour explorer des données et obtenir des visualisations
            └── visualize.R  <- Ex de script (pas inclus)

--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>

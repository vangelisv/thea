class(quality).
class(shape).
class(length).
class(mass).

subClassOf(shape,quality).
subClassOf(length,quality).
subClassOf(mass,quality).

class(anatomical_entity).
class(cell).
class(cell_part).

class(organ).
class(lung).
class(brain).

class(neuron).
class(pyramidal_neuron).
class(axon).

subClassOf(cell,anatomical_entity).
subClassOf(cell_part,anatomical_entity).
subClassOf(neuron,cell).
subClassOf(pyramidal_neuron,neuron).
subClassOf(axon,cell_part).
subClassOf(organ,anatomical_entity).
subClassOf(lung,organ).
subClassOf(brain,organ).

class(p1).
equivalentClasses([p1,intersectionOf([length,someValuesFrom(qualityOf,intersectionOf([axon,someValuesFrom(partOf,pyramidal_neuron)]))])]).
class(p2).
equivalentClasses([p2,intersectionOf([length,someValuesFrom(qualityOf,axon)])]).




import 'package:aeris/src/models/service.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/trigger.dart';

/// Object representation of a pipeline
class Pipeline {
  ///Unique identifier
  int id;

  /// Name of the pipeline, defined by the user
  final String name;

  /// How many times the pipeline was triggered
  final int triggerCount;

  /// Is the pipeline enabled
  bool enabled;

  ///The pipeline's reactions
  final List<Reaction> reactions;

  final Trigger trigger;
  Pipeline(
      {Key? key,
      required this.id,
      required this.name,
      required this.triggerCount,
      required this.enabled,
      required this.trigger,
      required this.reactions});

  /// Unserialize Pipeline from JSON
  static Pipeline fromJSON(Map<String, dynamic> data) {
    var action = data['action'] as Map<String, dynamic>;
    var reactions = data['reactions'] as List<dynamic>;

    return Pipeline(
        name: action['name'] as String,
        enabled: action['enabled'] as bool,
        id: action['id'] as int,
        triggerCount: action['triggerCount'] as int,
        trigger: Trigger.fromJSON(action),
        reactions: reactions
            .map<Reaction>((e) => Reaction.fromJSON(e))
            .toList());
  }

  /// Serialize Pipeline into JSON
  Object toJSON() => {
    "action": {
      "id": id,
      "name": name,
      "pType": trigger.name,
      "pParams": { for (var e in trigger.parameters) e.name : e.value }, ///Serialize
      "enabled": enabled,
      "lastTrigger": trigger.last?.toIso8601String(),
      "triggerCount": triggerCount
    }, 
    'reactions': reactions.map((e) => e.toJSON()).toList()
  };

  bool dependsOn(Service service) {
    return service == trigger.service || reactions.any((reaction) => reaction.service == service);
  }
}

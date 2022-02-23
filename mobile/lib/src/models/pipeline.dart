import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:flutter/material.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/trigger.dart';

/// Object representation of a pipeline
class Pipeline {
  ///Unique identifier
  final int id;

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
  static Pipeline fromJSON(Map<String, Object> data) {
    var action = data['action'] as Map<String, Object>;
    var reactions = data['reactions'] as Map<String, Object>;

    return Pipeline(
        name: action['name'] as String,
        enabled: action['enabled'] as bool,
        id: action['id'] as int,
        triggerCount: action['triggerCount'] as int,
        trigger: Trigger.fromJSON(action),
        reactions: (reactions as List<Object>)
            .map<Reaction>((e) => Reaction.fromJSON(e))
            .toList());
  }

  /// Serialize Pipeline into JSON
  Object toJSON() => {
    "action": {
      "name": name,
      "pType": aeris_action.Action.getType(trigger.service, trigger.name),
      "pParams": trigger.parameters,
      "enabled": enabled,
      "lastTrigger": trigger.last?.toIso8601String() ?? "0000-00-00",
      "triggerCount": triggerCount
    }, 
    'reactions': reactions.map((e) => e.toJSON()).toList()
  };
}

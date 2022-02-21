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
}

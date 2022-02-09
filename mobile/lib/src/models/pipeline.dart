import 'package:flutter/material.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/trigger.dart';

/// Object representation of a pipeline
class Pipeline {
  ///TODO Is the data type of the id correct?
  ///Unique identifier
  final int id;

  /// Name of the pipeline, defined by the user
  final String name;

  /// How many times the pipeline was triggered
  final int triggerCount;

  /// Is the pipeline enabled
  bool enabled;

  ///TODO Is the data type of the parameters correct?
  ///The parameters
  final Object parameters;

  ///The pipeline's reactions
  final List<Reaction> reactions;

  final Trigger trigger;
  Pipeline(
      {Key? key,
      required this.id,
      required this.name,
      required this.triggerCount,
      required this.enabled,
      required this.parameters,
      required this.trigger,
      required this.reactions});
}

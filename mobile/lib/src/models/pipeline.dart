import 'package:flutter/material.dart';

/// Object representation of a pipeline
class Pipeline {
  // TODO Is the data type of the id correct?
  // Unique identifier
  final int id;

  /// Name of the pipeline, defined by the user
  final String name;

  /// How many times the pipeline was triggered
  final int triggerCount;

  /// Last time the pipeline was triggered
  final DateTime lastTrigger;

  /// Is the pipeline enabled
  final bool enabled;

  // TODO Is the data type of the parameters correct?
  // The parameters
  final Object parameters;

  // TODO How is the reaction linked, of the triggering service?

  const Pipeline(
      {Key? key,
      required this.id,
      required this.name,
      required this.triggerCount,
      required this.lastTrigger,
      required this.enabled,
      required this.parameters});
}

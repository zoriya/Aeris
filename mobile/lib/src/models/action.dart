import 'package:flutter/widgets.dart';
import 'package:mobile/src/models/service.dart';

// Base class for reactions and trigger
abstract class Action {
  // Action's service
  final Service service;
  // Name fo the action
  final String name;
  // Action's parameters
  final Object parameters;
  const Action(
      {Key? key,
      required this.service,
      required this.name,
      this.parameters = const {}});
}

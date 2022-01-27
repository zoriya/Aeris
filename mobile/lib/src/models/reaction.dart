import 'package:flutter/widgets.dart';
import 'package:mobile/src/models/service.dart';

// Object representation of a reaction
class Reaction {
  // Reaction's service
  final Service service;
  // Name fo the reaction
  final String name;
  // Reaction's parameters
  final Object parameters;
  const Reaction({Key? key, required this.service, required this.name, required this.parameters});
}

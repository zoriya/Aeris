import 'package:mobile/src/models/pipeline.dart';
import 'package:flutter/cupertino.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';

/// Provider class for Pipelines
class PipelineProvider extends ChangeNotifier {
  /// List of Pipelines stored in Provider
  late List<Pipeline> pipelines;

  PipelineProvider() {
    var trigger1 = Trigger(
        service: const Service.spotify(),
        action: "Play song",
        last: DateTime.now());
    var trigger3 = Trigger(
        service: const Service.discord(),
        action: "Send a message",
        last: DateTime.now());
    var trigger2 = Trigger(
        service: const Service.spotify(),
        action: "Play song",
        last: DateTime.parse("2022-01-01"));
    var reaction = Reaction(
        service: Service.twitter(), parameters: {}, name: "Post a tweet");
    var pipeline1 = Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 10,
        enabled: true,
        parameters: {},
        trigger: trigger1,
        reactions: [reaction]);
    var pipeline2 = Pipeline(
        id: 10,
        name: "My very long action Action",
        triggerCount: 10,
        enabled: true,
        trigger: trigger2,
        parameters: {},
        reactions: [reaction, reaction]);
    var pipeline3 = Pipeline(
        id: 10,
        name: "Disabled",
        triggerCount: 10,
        enabled: false,
        trigger: trigger3,
        parameters: {},
        reactions: [reaction]);
    pipelines = [
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1
    ];
  }

  /// Adds a pipeline in the Provider
  addPipelineInProvider(Pipeline newPipeline) {
    pipelines.add(newPipeline);
    _sortsPipelines();
    notifyListeners();
  }

  /// Sets a new list of pipelines into the Provider
  setPipelineProvider(List<Pipeline> newPipelines) {
    pipelines = [];
    pipelines = newPipelines;
    _sortsPipelines();
  }

  _sortsPipelines() {
    pipelines.sort((a, b) {
      if (a.enabled == b.enabled) {
        return b.trigger.last.compareTo(a.trigger.last);
      }
      return b.enabled ? 1 : -1;
    });
  }

  /// Removes a specific pipeline from the Provider
  removePipelineFromProvider(int pipelineId) {
    for (Pipeline ppl in pipelines) {
      if (ppl.id == pipelineId) {
        // TODO: Remove the pipeline from the database or consult to do it out of the provider
        pipelines.remove(ppl);
        notifyListeners();
        return true;
      }
    }
    return false;
  }

  /// Removes every pipeline from the Provider
  clearProvider() {
    pipelines.clear();
    notifyListeners();
  }
}

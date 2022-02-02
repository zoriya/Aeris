import 'package:mobile/src/models/pipeline.dart';
import 'package:flutter/cupertino.dart';
import 'package:mobile/src/models/pipeline_collection.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';

/// Provider class for Pipelines
class PipelineProvider extends ChangeNotifier {
  /// List of Pipelines stored in Provider
  late PipelineCollection pipelineCollection;

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
        service: const Service.twitter(), parameters: {}, name: "Post a tweet");
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
    pipelineCollection = PipelineCollection(pipelines: [
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1
    ], sortingMethod: PipelineCollectionSort.last, sortingSplitDisabled: true);
  }

  /// Adds a pipeline in the Provider
  addPipelineInProvider(Pipeline newPipeline) {
    pipelineCollection.pipelines.add(newPipeline);
    sortsPipelines();
    notifyListeners();
  }

  /// Sets a new list of pipelines into the Provider
  setPipelineProvider(List<Pipeline> newPipelines) {
    pipelineCollection.pipelines = [];
    pipelineCollection.pipelines = newPipelines;
    sortsPipelines();
  }

  sortsPipelines() {
    pipelineCollection.sort();
    notifyListeners();
  }

  /// Removes a specific pipeline from the Provider
  removePipeline(Pipeline pipeline) {
    pipelineCollection.pipelines.remove(pipeline);
    notifyListeners();
  }

  /// Removes every pipeline from the Provider
  clearProvider() {
    pipelineCollection.pipelines.clear();
    notifyListeners();
  }
}

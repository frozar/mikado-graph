// Documentation link:
// https://hi.stamen.com/forcing-functions-inside-d3-v4-forces-and-layout-transitions-f3e89ee02d12

function constant(x) {
    return function() {
        return x;
    };
}

export function force () {
    let strength = constant(0.1);
    let nodes;
    let strengths;
    let defaultFixId = "root";
    let fixId        = defaultFixId;

    function force(alpha) {
        let node;
        let increment;
        let i;
        let n = nodes.length;
        for (i = 0, n = nodes.length; i < n; ++i) {
            if (nodes[i].id != fixId) {
                node = nodes[i];
                increment = -1 * strengths[i] * alpha;
                node.vy += increment;
            }
        }
    }

    function initialize() {
        if (!nodes)
            return;
        let n = nodes.length;
        strengths = new Array(n);
        let i;
        for (i = 0; i < n; ++i) {
            strengths[i] = +strength(nodes[i], i, nodes);
        }
    }

    force.initialize = function(nodesArg) {
        nodes = nodesArg;
        initialize();
    };

    force.strength = function(intensity) {
        if (arguments.length) {
            strength = (typeof intensity === "function" ? intensity : constant(+intensity));
            initialize();
            return force;
        }
        else {
            return strength;
        }
    };

    force.fixId = function(id) {
        if (arguments.length) {
            fixId = (typeof id === "string" ? id : defaultFixId);
            return force;
        }
        else {
            return fixId;
        }
    };

    return force;
}

const execSync = require('child_process').execSync;

const preCommit = (props) => {
  console.log('Setting version to ' + props.version);
  execSync('yq -i e ".version |= \\"0.' + props.version + '\\"" package.yaml && stack build --dry-run');
};

module.exports = {
  preCommit
}
